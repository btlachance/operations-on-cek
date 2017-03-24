#lang racket
(require
 racket/syntax
 syntax/parse
 syntax/id-set
 (for-template
  racket/base
  syntax/parse
  syntax/id-set))

(provide production step compile-grammar)
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
(module codegen-util racket/base
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

(require
 (for-template 'keywords 'codegen-util)
 (only-in 'keywords keywords-set))

(struct production (name forms)
  #:name -production
  #:constructor-name -production)
(define-syntax-class production
  #:attributes (name (form 1) data)
  #:literals (::=)
  (pattern (name:id ::= form ...+)
           #:attr data (-production #'name (attribute form))))

(define-syntax-class step
  #:attributes (lhs rhs implemented-by)
  #:literals (-->)
  (pattern [(~and lhs (e-l env-l k-l))
            -->
            (~and rhs (e-r env-r k-r))
            #:implemented-by form]
           #:with implemented-by #'form))


;; each form (nonterminal, literal, combination) needs associated with
;; its syntax class the following attributes
;; - compile-pattern
;; - compile-template
;; We may not need ones for the predicate/selector/constructor because
;; the compile-pattern and compile-template generation functions
;; should be the only part that needs to know about those

;; compile-combination : stx (listof stx) free-id-set
(define (compile-combination comb-name subforms)
  (define (subform->pattern form)
    (syntax-parse form
      ;; Note: this assumes any literals in the combination will have
      ;; a syntax class
      [i:id #'(~var _ i)]
      [(f ...) #`(#,(map subform->pattern #'(f ...)))]))

  ;; To build the compile function for the combination, each
  ;; subform needs a name. The name lets me get each subform's
  ;; compile function which I can then combine into the
  ;; combination's compile function.
  #`(define-syntax-class #,comb-name
      (pattern (#,@(map subform->pattern subforms))
               #:attr name #'#,comb-name)))

(define (compile-literal l)
  ;; we only allow literals in grammars that follow Racket's
  ;; identifier rules; e.g. we don't allow number literals here
  #`(define-syntax-class #,l
      ;; TODO add literal's attributes for associated code/data
      (pattern (~literal #,l)
               #:attr name #'#,l)))

(define (compile-production nonterminal forms)
  (define (form->pattern form)
    ;; TODO bubble up attributes
    #`(pattern (~var f #,form)
               #:attr name (attribute f.name)))
  (define-values (compiled-combinations variant-patterns)
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
  #`(begin
      (define-syntax-class #,nonterminal
        ;; TODO add nonterminal's attributes for associated code/data
        (pattern (~var _ (pattern-metavar #'#,nonterminal))
                 #:attr name #'#,nonterminal)
        #,@variant-patterns)
      #,@compiled-combinations))

;; This mess of ->literals functions could probably be cleaned up
;; if I didn't try to compile all the literals in one go; something
;; more compositional do the same trick.

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

;; (listof -production) -> stx
(define (compile-grammar productions)
  (define nonterminals (immutable-free-id-set (map production-name productions)))
  ;; see HACK below
  (define (keyword? form)
    (and (identifier? form) (identifier-binding form)))
  (define (literal? form)
    (and (identifier? form)
         (not (free-id-set-member? nonterminals form))
         ;; (not (free-id-set-member? keywords-set form))
         ;; HACK I wanted to use keywords-set like in the line above
         ;; to make sure that the keywords didn't get interpreted as
         ;; literals. I couldn't get that to work, so I'm instead
         ;; relying on them being the only identifiers mentioned on
         ;; the RHS of a production that are already bound (e.g.
         ;; literals and other variables aren't yet bound at grammar
         ;; compilation time)
         (not (keyword? form))))
  (define literals
    (immutable-free-id-set (productions->literals productions literal?)))

  #`(begin
      #,@(for/list ([p productions])
           (compile-production
            (production-name p)
            (filter-not keyword? (production-forms p))))
      #,@(set-map literals compile-literal)))

;; TODO whatever procedure compiles the transition relation/#:steps to
;; IR needs to know what the toplevel syntax classes are for
;; expressions, environments, and continuations.
