#lang racket
(require
 racket/syntax
 syntax/parse
 syntax/id-set
 (for-template
  racket/base
  syntax/parse
  syntax/id-set))

(provide production step compile-cek)
(module+ test
  (require rackunit))

(module keywords racket/base
  (require (for-syntax racket/base) syntax/id-set)
  (define-syntax (keywords stx)
    (syntax-case stx ()
      [(_ id ...)
       (begin
         #`(begin
             (define-syntax (id stx)
               (raise-syntax-error 'cek-metalang "out of context")) ...
               (provide id) ...
               (define keywords-set
                 (immutable-free-id-set (list #'id ...)))
               (provide keywords-set)))]))
  ;; TODO Keywords need to be treated specially in patterns. At the
  ;; very least, the compiler needs to know what their meaning is.
  (keywords ::= -->
            natural
            default-env lookup extend
            default-mt ::))
(module compile-util racket/base
  (require racket/match syntax/parse)
  (provide pattern-metavar)
  ;; TODO Things like natural don't seem like metavariables, so if I
  ;; want the natural pattern to allow underscores (and I think I do
  ;; want that) then I should change the name of this and the
  ;; corresponding syntax class.
  (define (matches-metavar? pattern id)
    (define without-suffix
      (match (symbol->string (syntax-e pattern))
        [(regexp #px"([^_]+)(_.+)?" (list _ contents suffix))
         (define symbol-without-suffix (string->symbol contents))
         (datum->syntax pattern symbol-without-suffix pattern)]))
    (and (identifier-binding id)
         (bound-identifier=? without-suffix id)))

  (define-syntax-class (pattern-metavar id)
    (pattern x:id
             #:when (matches-metavar? #'x id))))

(module codegen-util racket/base
  (provide pattern-name err:expected-x-in-y)
  ;; pattern-name : identifier -> IR
  ;; given a pattern that binds a single variable, produces an
  ;; identifier in IR that can be used to bind that variable
  (define (pattern-name p)
    p)

  ;; err:expected-x-in-y pattern id -> IR
  ;; given a pattern x and an identifier y, produces IR for an error
  ;; message when the value corresponding to pattern x was not found
  ;; in y
  (define (err:expected-x-in-y x y)
    (define msg
      (format "expected ~a in ~a but found something else"
              (syntax->datum x)
              (syntax->datum y)))
    #`(error 'runtime #,msg)))

(require
 (for-template 'keywords 'compile-util 'codegen-util)
 (only-in 'keywords keywords-set))

(struct production (name forms)
  #:name -production
  #:constructor-name -production)
(define-syntax-class production
  #:attributes (name (form 1) data)
  #:literals (::=)
  (pattern (name:id ::= form ...+)
           #:attr data (-production #'name (attribute form))))

(struct step (lhs rhs implemented-by)
  #:name -step
  #:constructor-name -step)
(define-syntax-class step
  #:attributes (lhs rhs implemented-by data)
  #:literals (-->)
  (pattern [(~and lhs (e-l env-l k-l))
            -->
            (~and rhs (e-r env-r k-r))
            #:implemented-by form]
           #:with implemented-by #'form
           #:attr data (-step #'lhs #'rhs (attribute implemented-by))))

;; compile-combination : stx (listof stx) free-id-set
;; generates code using the idgetinfo
(define (compile-combination comb-name subforms)
  (define (subform->pattern form)
    (syntax-parse form
      ;; Note: this assumes any literals in the combination will have
      ;; a syntax class
      [i:id #'(~var _ i)]
      [(f ...) #`(#,(map subform->pattern #'(f ...)))]))

  (define-values (subform-patterns names)
    (for/lists (subform-patterns names)
               ([subform subforms])
      (values
       (subform->pattern subform)
       (format-id comb-name "~a-sub~a" comb-name (length names)))))

  (define info
    #`(begin
        (define pred-id #'#,(format-id comb-name "~a?" comb-name))
        (lambda (pattern source rest)
          (define (compile-subpattern subpattern name body)
            (define compile (#,(getinfo) subpattern))
            (compile subpattern #`(#,name #,source) body))
          (define body
            (foldr
             compile-subpattern
             rest
             (syntax->list pattern)
             ;; constructing this list by splicing in names
             ;; feels very odd; maybe the names belong in
             ;; some other part of the combination's info?
             (syntax->list #'(#,@names))))
          
          #`(if (not (#,pred-id #,source))
                #,(err:expected-x-in-y #'comb-name source)
                #,body))))

  #`(define-syntax-class #,comb-name
      (pattern (#,@subform-patterns)
               #:attr name #'#,comb-name
               #:attr info #,info)))

(define (compile-literal l)
  (define info-id (format-id l "~a-info" l))
  (define info
    #`(begin
        (define pred-id #'#,(format-id l "~a?" l))
        (lambda (pattern source rest)
          (define name (pattern-name pattern))
          #`(if (not (#,pred-id #,source))
                #,(err:expected-x-in-y pattern source)
                (let ([#,name #,source])
                  #,rest)))))

  #`(begin
      ;; we only allow literals in grammars that follow Racket's
      ;; identifier rules; e.g. we don't allow number literals here
      ;; if/when we do want number literals via compile-literal, then
      ;; we'll definitely need more than just a fixed pred-id
      (define-syntax-class #,l
        (pattern (~literal #,l)
                 #:attr name #'#,l
                 #:attr info #,info))))

(define (compile-production nonterminal forms)
  (define (form->pattern form)
    #`(pattern (~var f #,form)
               #:attr name (attribute f.name)
               #:attr info (attribute f.info)))
  (define-values (compiled-combinations variant-patterns)
    ;; It would be nice to use for/lists here instead of for/fold but
    ;; it's not clear to me how we could use it; we don't always
    ;; produce an item for compiled-combinations yet we do always
    ;; produce an item for variant-patterns
    (for/fold ([compiled-combinations '()]
               [variant-patterns '()])
              ([form forms])
      (syntax-parse form
        [i:id
         (values
          compiled-combinations
          (cons (form->pattern #'i) variant-patterns))]
        [(f ...)
         (define name (format-id nonterminal
                                 "~a-~a~a"
                                 ;; e.g. e-combination0
                                 nonterminal
                                 'combination
                                 (length compiled-combinations)))
         (values
          (cons (compile-combination name (attribute f)) compiled-combinations)
          (cons (form->pattern name) variant-patterns))])))

  (define info
    ;; For now, info only contains the compile function
    #`(begin
        (define pred-id #'#,(format-id nonterminal "~a?" nonterminal))
        (lambda (pattern source rest)
          (define name (pattern-name pattern))
          ;; here we use syntax objects as IR which already
          ;; seems a bit confusing
          #`(if (not (#,pred-id #,source))
                #,(err:expected-x-in-y pattern source)
                (let ([#,name #,source])
                  #,rest)))))
  #`(begin
      (define-syntax-class #,nonterminal
        (pattern (~var _ (pattern-metavar #'#,nonterminal))
                 #:attr name #'#,nonterminal
                 #:attr info #,info)

        #,@variant-patterns)
      #,@compiled-combinations))

(define (productions->literals productions literal?)
  (define (production->literals p)
    (apply append (map form->literals (production-forms p))))
  (define (form->literals form)
    (syntax-parse form
      [i:id (if (literal? #'i)
                (list #'i)
                '())]
      [(f ...)
       (apply append (map form->literals (attribute f)))]))

  (apply append (map production->literals productions)))


;; compile-getinfo : id (listof id) -> stx
;; generates code for a function `name` which, when given an arbitrary
;; pattern that is an instances of at least one of the nonterminals or
;; a term in the general pattern language, produces the pattern's
;; info object
(define (compile-getinfo name nonterminals)
  #`(begin
      (define-syntax-class toplevel
        #:attributes (info)
        (pattern (~literal toplevel)
                 #:attr info #f)
        #,@(for/list ([nonterminal nonterminals])
             #`(pattern (~var p #,nonterminal)
                        #:attr info (attribute p.info))))
      (define (#,name pattern)
        (syntax-parse pattern
          [(~var p toplevel)
           (or (attribute p.info)
               (error
                'cek-metalang-lib
                "attempting to ~a for ~a failed; did toplevel syntax class escape?"
                (syntax-e #'name)
                pattern))]))))

(define getinfo (make-parameter #'getinfo))

;; (listof -production) -> stx
(define (compile-grammar productions)
  (define nonterminals (immutable-free-id-set (map production-name productions)))
  ;; see HACK below
  (define (keyword? form)
    (and (identifier? form) (identifier-binding form)))
  (define (literal? form)
    (and (identifier? form)
         (not (free-id-set-member? nonterminals form))
         ;; (not (free-id-set-member? keywords-set form)) HACK I
         ;; wanted to use keywords-set (like I use nonterminals in the
         ;; line above) to make sure that the keywords didn't get
         ;; interpreted as literals. I couldn't get that to work, so
         ;; I'm instead relying on them being the only identifiers
         ;; mentioned on the RHS of a production that are already
         ;; bound (e.g. literals and other variables aren't yet bound
         ;; at grammar compilation time)
         (not (keyword? form))))
  (define literals
    (immutable-free-id-set (productions->literals productions literal?)))

  #`(begin
      #,(compile-getinfo (getinfo) nonterminals)
      #,@(for/list ([p productions])
           (compile-production
            (production-name p)
            (filter-not keyword? (production-forms p))))
      #,@(set-map literals compile-literal)))

(define (compile-transition steps)
  #'(void))

(define (compile-cek name productions steps)
  (parameterize ([getinfo (generate-temporary 'getinfo)])
  #`(begin
      #,(compile-grammar productions)
      #,(compile-transition steps))))

;; TODO whatever procedure compiles the transition relation/#:steps to
;; IR needs to know what the toplevel syntax classes are for
;; expressions, environments, and continuations.
