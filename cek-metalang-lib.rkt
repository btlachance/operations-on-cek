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

(module codegen-util racket/base
  (provide
   pattern-name
   err:expected-x-in-y
   (struct-out info)
   getinfo-id)
  ;; pattern-name : identifier -> IR
  ;; given a pattern that binds a single variable, produces an
  ;; identifier in IR that can be used to bind that variable
  (define (pattern-name p)
    p)

  (struct info (compile-pattern compile-template))

  ;; err:expected-x-in-y pattern id -> IR
  ;; given a pattern x and an identifier y, produces IR for an error
  ;; message when the value corresponding to pattern x was not found
  ;; in y
  (define (err:expected-x-in-y x y)
    (define msg
      (format "expected ~a in ~a but found something else"
              (syntax->datum x)
              (syntax->datum y)))
    #`(error 'runtime #,msg))
  (define getinfo-id #'getinfo))

(module keywords racket/base
  (require
   (for-syntax racket/base)
   syntax/id-set
   syntax/parse)
  (provide make-keywords-delta-introducer)
  (define-syntax (keywords stx)
    (syntax-case stx ()
      [(_ id ...)
       (begin
         #`(begin
             (define-syntax (id stx)
               (raise-syntax-error 'cek-metalang "out of context" stx)) ...
               (provide id) ...
               (define keywords-set
                 (immutable-free-id-set (list #'id ...)))
               (provide keywords-set)))]))
  (keywords ::= -->
            natural
            default-env lookup extend)
  (define (make-keywords-delta-introducer ext-stx)
    (make-syntax-delta-introducer ext-stx #'::)))

(module prims racket/base
  ;; HACK I don't yet know a better way to do this, but this whole
  ;; module seems like one big hack
  (require
   (submod ".." codegen-util)
   (for-template
    (submod ".." keywords)
    (submod ".." codegen-util)
    racket/syntax
    syntax/parse
    racket/base))

  (provide prim+class-stx)
  (define prim+class-stx
    (list
     (cons
      #'lookup
      #`(define-syntax-class lookup-class
          (pattern ((~literal lookup) e x)
                   #:attr name #'lookup
                   #:attr info
                   (begin
                     (define (compile-pattern pattern rest)
                       (raise-syntax-error
                        #f
                        "attempted to use lookup as a pattern when it can only be used as a template"
                        this-syntax))
                     (define (compile-template template dest rest)
                       (define (compile-subtemplate subtemplate dest rest)
                         (define compile-sub (info-compile-template (#,getinfo-id subtemplate)))
                         (compile-sub subtemplate dest rest))
                       (define subtemplates (cdr (syntax->list template)))
                       (define temps (map generate-temporary subtemplates))

                       (foldr
                        compile-subtemplate
                        #`(let ([#,dest (prim-lookup #,@temps)])
                            #,rest)
                        subtemplates
                        temps))
                     (info compile-pattern compile-template)))))
     (cons
      #'extend
      #`(define-syntax-class extend-class
          (pattern ((~literal extend) e x v)
                   #:attr name #'extend
                   #:attr info
                   (begin
                     (define (compile-pattern pattern rest)
                       (raise-syntax-error
                        #f
                        "attempted to use lookup as a pattern when it can only be used as a template"
                        this-syntax))
                     (define (compile-template template dest rest)
                       (define (compile-subtemplate subtemplate dest rest)
                         (define compile-sub (info-compile-template (#,getinfo-id subtemplate)))
                         (compile-sub subtemplate dest rest))
                       (define subtemplates (cdr (syntax->list template)))
                       (define temps (map generate-temporary subtemplates))

                       (foldr
                        compile-subtemplate
                        #`(let ([#,dest (prim-extend #,@temps)])
                            #,rest)
                        subtemplates
                        temps))
                     (info compile-pattern compile-template))))))))

(module compile-util racket/base
  (require racket/match syntax/parse)
  (provide pattern-metavar)
  (define (matches-metavar? pattern id)
    (define without-suffix
      (match (symbol->string (syntax-e pattern))
        [(regexp #px"([^_]*)(_.+)?" (list _ contents suffix))
         (define symbol-without-suffix (string->symbol contents))
         (datum->syntax pattern symbol-without-suffix pattern)]))
    (and (identifier-binding id)
         (bound-identifier=? without-suffix id)))

  (define-syntax-class (pattern-metavar id)
    #:description (format "pattern ~a" (syntax-e id))
    (pattern x:id
             #:when (matches-metavar? #'x id))))

(require
 (for-template 'keywords 'compile-util 'codegen-util racket/pretty)
 (only-in 'keywords keywords-set make-keywords-delta-introducer)
 (only-in 'prims prim+class-stx)
 (only-in 'codegen-util getinfo-id)
 ;; for new representations
 'compile-util)


(struct production (name forms)
  #:name -production
  #:constructor-name -production)
(define-syntax-class production
  #:attributes (name (form 1) data)
  #:literals (::=)
  (pattern (name:id ::= form ...+)
           #:attr data (-production #'name (attribute form))))

(struct step (lhs rhs)
  #:name -step
  #:constructor-name -step)
(define-syntax-class step
  #:attributes (lhs rhs data)
  #:literals (-->)
  (pattern [(~and lhs (e-l env-l k-l))
            -->
            (~and rhs (e-r env-r k-r))]
           #:attr data (-step #'lhs #'rhs)))

;; compile-combination : stx (listof stx) free-id-set
;; generates code using the getinfo-id
(define (compile-combination comb-name subforms)
  (define (subform->pattern form)
    (syntax-parse form
      ;; Note: this assumes any literals in the combination will have
      ;; a syntax class
      [i:id #'(~var _ i)]
      [(f ...) #`(#,(map subform->pattern #'(f ...)))]))

  ;; TODO `names` aren't just names---they're the accessor/projection
  ;; names. Renaming them to something more meaningful will make the
  ;; body of compile-subpattern more clear
  (define-values (subform-patterns names)
    (for/lists (subform-patterns names)
               ([subform subforms])
      (values
       (subform->pattern subform)
       (format-id comb-name "~a-sub~a" comb-name (length names)))))

  (define info-stx
    #`(begin
        (define pred-id #'#,(format-id comb-name "~a?" comb-name))
        (define (compile-pattern pattern source rest)
          (define (compile-subpattern subpattern name body)
            (define compile-sub (info-compile-pattern (#,getinfo-id subpattern)))
            (compile-sub subpattern #`(#,name #,source) body))
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
                #,(err:expected-x-in-y #'#,comb-name source)
                #,body))
        (define constructor-id #'#,(format-id comb-name "make-~a" comb-name))
        (define (compile-template template dest rest)
          (define (compile-subtemplate subtemplate dest rest)
            (define compile-sub (info-compile-template (#,getinfo-id subtemplate)))
            (compile-sub subtemplate dest rest))
          (define subtemplates (syntax->list template))
          (define temps (generate-temporaries template))

          (foldr
           compile-subtemplate
           #`(let ([#,dest (#,constructor-id #,@temps)])
               #,rest)
           subtemplates
           temps))
        (info compile-pattern compile-template)))

  #`(define-syntax-class #,comb-name
      #:description #,(format "~a" (map syntax->datum subforms))
      (pattern (#,@subform-patterns)
               #:attr name #'#,comb-name
               #:attr info #,info-stx)))

(define (compile-literal l)
  (define info-stx
    #`(begin
        (define pred-id #'#,(format-id l "~a?" l))
        (define (compile-pattern pattern source rest)
          (define name (pattern-name pattern))
          #`(if (not (#,pred-id #,source))
                #,(err:expected-x-in-y pattern source)
                (let ([#,name #,source])
                  #,rest)))
        (define (compile-template template dest rest)
          #`(let ([#,dest #,template])
              #,rest))
        (info compile-pattern compile-template)))

  #`(begin
      ;; we only allow literals in grammars that follow Racket's
      ;; identifier rules; e.g. we don't allow number literals here
      ;; if/when we do want number literals via compile-literal, then
      ;; we'll definitely need more than just a fixed pred-id
      (define-syntax-class #,l
        #:description #,(format "literal ~a" (syntax-e l))
        (pattern (~literal #,l)
                 #:attr name #'#,l
                 #:attr info #,info-stx))))

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

  (define info-stx
    #`(begin
        (define pred-id #'#,(format-id nonterminal "~a?" nonterminal))
        (define (compile-pattern pattern source rest)
          (define name (pattern-name pattern))
          ;; here we use syntax objects as IR which already
          ;; seems a bit confusing
          #`(if (not (#,pred-id #,source))
                #,(err:expected-x-in-y pattern source)
                (let ([#,name #,source])
                  #,rest)))
        (define (compile-template template dest rest)
          #`(let ([#,dest #,template])
              #,rest))
        (info compile-pattern compile-template)))
  #`(begin
      (define-syntax-class #,nonterminal
        #:description #,(format "nonterminal ~a" (syntax-e nonterminal))
        (pattern (~var _ (pattern-metavar #'#,nonterminal))
                 #:attr name #'#,nonterminal
                 #:attr info #,info-stx)
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


;; compile-getinfo : id (listof id) (listof id) (listof id) -> stx
;; generates code for a function `name` which, when given an arbitrary
;; pattern that is an instances of at least one of the nonterminals or
;; a term in the general pattern language, produces the pattern's
;; info object
(define (compile-getinfo name nonterminals literals prims)
  (define toplevel-id (format-id name "toplevel-~a" name))

  (define (prim->prim-class id)
    ;; must follow what prim+class-stx does
    (format-id id "~a-class" id))
  (define keyword-classes (map prim->prim-class prims))

  #`(begin
      (define-syntax-class #,toplevel-id
        #:description #,(format "a member of the ~a class" (syntax-e name))
        #:attributes (info name)
        (pattern (~literal #,toplevel-id)
                 #:attr info #f
                 #:attr name 'toplevel)
        ;; HACK when we try to match a literal in part of a
        ;; combination, we try to get the literal's info via
        ;; getinfo. But, the literals that only appear inside a
        ;; combination are lifted out like other literals---the two
        ;; differ though in that "other literals" appear as a RHS of a
        ;; nonterminal. Thus, there's a path from the toplevel to the
        ;; literal via the literal's nonterminal. The literals that
        ;; only appear inside a combination though have no such
        ;; nonterminal; that's why we have to add them as subpatterns
        ;; of toplevel.
        #,@(for/list ([sub (in-sequences nonterminals literals keyword-classes)])
             #`(pattern (~var p #,sub)
                        #:attr info (attribute p.info)
                        #:attr name (attribute p.name)))
        #,@(for/list ([prim prims])
             #`(pattern (~literal #,prim)
                        #:attr info #f
                        #:attr name #'#,prim)))
      (define (#,getinfo-id pattern)
        (syntax-parse pattern
          [(~var p #,toplevel-id)
           (or (attribute p.info)
               (error
                'cek-metalang-internal
                "attempting to ~a for ~a failed; did toplevel syntax class escape?"
                #,(format "~s" (syntax-e getinfo-id))
                (syntax->datum pattern)))]))))

;; id (listof -production) -> stx
(define (compile-grammar name productions)
  ;; assumes that no nonterminal redefines a keyword
  (define nonterminals (immutable-free-id-set (map production-name productions)))
  (define keyword-introducer (make-keywords-delta-introducer (free-id-set-first nonterminals)))

  ;; keyword? : stx -> boolean
  (define (keyword? form)
    (and (identifier? form)
         ;; I'm not confident that this is right; I don't totally
         ;; understand scopes/delta-introducers yet
         (free-id-set-member? keywords-set (keyword-introducer form 'remove))))

  (define (literal? form)
    (and (identifier? form)
         (not (free-id-set-member? nonterminals form))
         (not (keyword? form))))

  (define literals
    (immutable-free-id-set (productions->literals productions literal?)))

  (define prims (map car prim+class-stx))

  #`(begin
      #,(compile-getinfo name nonterminals literals prims)
      #,@(map cdr prim+class-stx)
      ;; this won't do what the user expected if they named a
      ;; production after a keyword---since we filter out keywords
      ;; here, it won't compile the keyword-named production
      #,@(for/list ([p productions])
           (compile-production
            (production-name p)
            (filter-not keyword? (production-forms p))))
      #,@(set-map literals compile-literal)))

(define (compile-transition c-id e-id k-id steps)
  ;; we assume that a step's lhs/rhs is a sequence of
  ;; patterns/templates that are recognizable by our AST
  #`(begin
      (define (compile-lhs subpatterns arg-names rest)
        (foldr
         (lambda (pattern source r)
           (define compile-pattern (info-compile-pattern (#,getinfo-id pattern)))
           (compile-pattern pattern source r))
         rest
         subpatterns
         arg-names))
      (define (compile-rhs subtemplates dest-names rest)
        (foldr
         (lambda (template dest r)
           (define compile-template (info-compile-template (#,getinfo-id template)))
           (compile-template template dest r))
         rest
         subtemplates
         dest-names))

      (define (compile-step fn-name lhs rhs)
        (define arg-names (generate-temporaries lhs))
        (define dest-names (generate-temporaries rhs))
        (define body
          (compile-lhs (syntax->list lhs) arg-names
                       (compile-rhs (syntax->list rhs) dest-names #`(values #,@dest-names))))
        (define result
          #`(define (#,fn-name #,@arg-names)
              #,body))
        (pretty-print (syntax->datum result))
        result)

      #,@(for/list ([step steps]
                    [n (in-naturals)])
           (define fn-name (format-id (step-lhs step) "step~a" n))
           #`(compile-step #'#,fn-name #'#,(step-lhs step) #'#,(step-rhs step)))))

;; a name is a symbol

;; a shape is one of
;; - symbol, representing a literal
;; - (sequence name (listof field) stx)
;; - (choice name (U #f (nonemptylistof shape)) id)
;;   * a choice's alternatives is only #f while parsing the grammar;
;;     once it is set to a non-#f value it must not be changed
;;   * TODO I probably also want a restriction on what's in the list
;;   of alternatives. For example, I don't want a choice be able to
;;   produce itself without going through a sequence and I don't want
;;   a choice's name to be in its list of alternatives either. The
;;   latter couldn't occur during grammar generation, as the name
;;   would always be resolved to the choice, but it's an odd
;;   possibility in the data structure choice.
(struct sequence (name fields stx))
(struct choice (name [alternatives #:mutable] nonterminal))

;; a field is a (field name shape)
(struct field (name shape))
;; I don't think fields with shape symbol need to be represented in
;; our classes; I think they're only artifacts of the concrete syntax
;; and aren't useful for the abstract syntax or classes we generate.
;; Maybe this distinction means field isn't the best name for this

;; shape->name : shape -> name
(define (shape->name shape)
  (cond
    [(symbol? shape) shape]
    [(sequence? shape) (sequence-name shape)]
    [else (choice-name shape)]))

;; sequence-of : (listof field)
;; recognizes when a syntax matches a sequence of the given fields
(define-syntax-class (sequence-of fields)
  #:attributes ([names 1])
  (pattern (~or (~and () (~fail #:unless (empty? fields))
                      (~bind [(names 1) '()]))
                (~and (~fail #:when (empty? fields))
                      ((~var s (form-matching (field-shape (car fields))))
                       .
                       (~var ss (sequence-of (cdr fields))))
                      (~bind ([names 1] (cons (attribute s.name) (attribute ss.names))))))))

;; one-of : (listof shape)
;; recognizes when a syntax matches one of the alternatives in a
;; choice
(define-syntax-class (one-of shapes)
  #:attributes (name)
  (pattern (~and (~fail #:when (empty? shapes))
                 (~or (~var s1 (form-matching (car shapes)))
                      (~var s2 (one-of (cdr shapes))))
                 (~bind [name (or (attribute s1.name) (attribute s2.name))]))))

;; form-matching : shape
;; recognizes when a syntax matches the given shape
(define-syntax-class (form-matching shape)
  #:description (format "form matching ~a" (shape->name shape))
  #:attributes (name)
  (pattern (~or (~and (~fail #:unless (symbol? shape))
                      id
                      (~fail #:unless (equal? shape (syntax-e #'id)))
                      (~bind [name (syntax-e #'id)]))
                (~and (~fail #:unless (sequence? shape))
                      (~var _ (sequence-of (sequence-fields shape)))
                      (~bind [name (sequence-name shape)]))
                (~and (~fail #:unless (choice? shape))
                      (~or (~and (~var s (pattern-metavar (choice-nonterminal shape)))
                                 (~bind [name (choice-name shape)]))
                           (~and (~var ss (one-of (choice-alternatives shape)))
                                 (~bind [name (attribute ss.name)])))))))

(module+ test
  (require rackunit)

  (define sandwich?
    (syntax-parser
      [(~var _ (form-matching 'sandwich)) #t]
      [_ #f]))
  (check-true (sandwich? #'sandwich))
  (check-false (sandwich? #'apple))
  (check-false (sandwich? #'(sandwich)))
  (check-false (sandwich? #'()))

  (define empty-seq?
    (syntax-parser
      [(~var _ (sequence-of '())) #t]
      [_ #f]))
  (check-true (empty-seq? #'()))
  (check-false (empty-seq? #'a))
  (check-false (empty-seq? #'(sandwich)))

  (define spinach-singleton-seq?
    (syntax-parser
      [(~var _ (sequence-of (list (field 'greens 'spinach)))) #t]
      [_ #f]))
  (check-true (spinach-singleton-seq? #'(spinach)))
  (check-false (spinach-singleton-seq? #'x))
  (check-false (spinach-singleton-seq? #'()))

  (define groceries-multi-seq?
    (syntax-parser
      [(~var _ (sequence-of (list (field 'snack 'spinach)
                                  (field 'dessert 'chocolate)
                                  (field 'spice 'pepper))))
       #t]
      [_ #f]))
  (check-true (groceries-multi-seq? #'(spinach chocolate pepper)))
  (check-false (groceries-multi-seq? #'(grapes bad pepper)))

  ;; I give the sym/id names because racket-mode doesn't lex the empty
  ;; symbols quite right which messes up indentation/highlighting
  (define empty-sym '||)
  (define empty-id #'||)
  (define || (choice empty-sym (list 'mt) empty-id))
  (define ||?
    (syntax-parser
      [(~var _ (form-matching ||)) #t]
      [_ #f]))
  (check-false (||? #'beer))
  (check-true (||? #'||))
  (check-true (||? #'mt))
  (check-true (||? #'||_1))

  (define dog (choice 'dog (list 'dogpet) #'dog))
  (define cat (choice 'cat (list 'catpet) #'cat))
  (define cat?
    (syntax-parser
      [(~var _ (form-matching cat)) #t]
      [_ #f]))
  (define choice-of-choices (choice 'pet (list dog cat) #'pet))
  (define pet?
    (syntax-parser
      [(~var _ (form-matching choice-of-choices)) #t]
      [_ #f]))
  (check-true (pet? #'cat_1))
  (check-true (pet? #'dog))
  (check-true (pet? #'dogpet))
  (check-false (cat? #'dog))

  (define one-of-syms?
    (syntax-parser
      [(~var _ (one-of (list 'x 'y 'z))) #t]
      [_ #f]))
  (check-true (one-of-syms? #'x))
  (check-false (one-of-syms? #'(x y z)))
  (check-false (one-of-syms? #'()))

  (define e (choice 'e #f #'e))
  (define var 'var)
  (define lam (sequence 'lam (list (field 'tag 'lam) (field 'x var) (field 'e e)) #'lamstx))
  (define app (sequence 'app (list (field 'e1 e) (field 'e2 e)) #'appstx))
  (set-choice-alternatives! e (list var lam app))
  (define e?
    (syntax-parser
      [(~var _ (form-matching e)) #t]
      [_ #f]))
  (check-true (e? #'(e_1 e_2)))
  (check-true (e? #'var))
  (check-false (e? #'var_1))
  (check-false (e? #'x))
  (check-true (e? #'(lam var var)))
  (check-false (e? #'((lam var var))))
  (check-true (e? #'(lam var (lam var var))))
  (check-false (e? #'(lam var)))
  (define app?
    (syntax-parser
      [(~var _ (form-matching app)) #t]
      [_ #f]))
  (define app1 #'((lam var var) (lam var var)))
  (check-true (e? app1))
  (check-true (app? app1))

  (define (e-form->name form)
    (syntax-parse form
      [(~var f (form-matching e))
       (attribute f.name)]
      [_ #f]))
  (check-equal? (e-form->name #'e_1) 'e)
  (check-equal? (e-form->name #'(lam var var)) 'lam)
  (check-equal? (e-form->name #'var) 'var)
  (check-equal? (e-form->name #'((lam var var) (lam var var))) 'app))

;; name->shape : name (map name shape) -> shape
(define (name->shape name shape-map)
  (or (hash-ref shape-map name #f)
      name))

;; comb->shape : name (map name shape) (listof identifier) -> shape
(define (comb->shape name shape-map subforms)
  (define-values (subshapes fields)
    (for/lists (subshapes fields)
               ([subform subforms])
      (define sub (name->shape (syntax-e subform) shape-map))
      (define f (field (gensym (shape->name sub)) sub))
      (values sub f)))

  (sequence name fields #'here))

;; production-forms->shapes : name (map name shape) (listof stx)
;; Produces a list of shapes for each of the given forms. When a
;; form is a combination, p-name is used to format the name of the
;; resulting sequence shape
(define (production-forms->shapes p-name shape-map forms)
  (for/list ([form forms]
             [i (in-naturals)])
    (cond
      [(identifier? form)
       (name->shape (syntax-e form) shape-map)]
      [else
       (define subforms (syntax->list form))
       (unless (andmap identifier? subforms)
         (raise-syntax-error
          'compile-grammar2
          "cannot nest a combination in a combination"
          form))
       (define comb-name (format-symbol "~a-comb~a" p-name i))
       (comb->shape comb-name shape-map subforms)])))

(module+ test
  (define (form-with-nested-comb)
    (define snack-map (hash 'snack (choice 'snack #f #'here)))
    (define form #'(bagof (potatoes)))
    (production-forms->shapes
     'snack
     snack-map
     (list form)))
  (check-exn exn:fail:syntax? form-with-nested-comb))

;; compile-grammar2 : productions -> (map name shape)
;; produces a map from names to shapes for the given grammar
(define (compile-grammar2 productions)
  (define init-choice-shapes
    (for/hash ([p productions])
      (define id (production-name p))
      (define shape (choice (syntax-e id) #f id))
      (values (syntax-e id) shape)))

  (for/fold ([shape-map init-choice-shapes])
            ([p productions])
    (define p-name (syntax-e (production-name p)))
    (define shapes (production-forms->shapes p-name shape-map (production-forms p)))

    ;; we can tie the knot once we have all of p's right-hand-side
    ;; shapes (which may themselves refer to p)
    (define p-choice (hash-ref shape-map p-name))
    (set-choice-alternatives! p-choice shapes)

    (define (new-shape? s)
      (not (hash-has-key? shape-map (shape->name s))))
    (define new-shapes (filter new-shape? shapes))

    (for/fold ([shape-map shape-map])
              ([new-shape new-shapes])
      (hash-set shape-map (shape->name new-shape) new-shape))))

(module+ test
  (define snacks-grammar
    (list (-production #'snack (list #'(bagof chips)
                                     #'spinach))
          (-production #'chips (list #'chocolate
                                     #'potato
                                     #'kale))))
  (define snacks-shape-map (compile-grammar2 snacks-grammar))
  (check-true (hash-has-key? snacks-shape-map 'chocolate))
  (check-true (hash-has-key? snacks-shape-map 'chips))
  (check-equal? (length (hash-keys snacks-shape-map)) 7))

;; a IR is one of
;; - (ir:check-instance name name IR)
;; - (ir:let (listof (list name simple-IR)) IR)
;; - (ir:return (listof name))
(struct ir:check-instance (arg class-name rest) #:transparent)
(struct ir:let (bindings rest) #:transparent)
(struct ir:return (results)  #:transparent)

;; a simple-IR is one of
;; - name
;; - (ir:make name (listof name))
;; - (ir:project name name name)
;; - (ir:prim name (listof name))
(struct ir:make (class-name args) #:transparent)
(struct ir:project (class-name field-name arg) #:transparent)
(struct ir:prim (name args) #:transparent)

;; a class is a (class name
;;                     (listof class-field)
;;                     (pattern source (listof name) IR -> (list IR (listof name)))
;;                     (template dest (listof name) IR -> IR))
;; where compile-pattern assumes the pattern is an instance of the
;; class' shape; ditto for compile-template and templates.
;; Invariants:
;; - name must be equal? to the class' shape's name
;; - Callers of compile-pattern must thread the resulting (listof
;;   name) through to subsequent calls of compile-pattern; this is
;;   because compiling a pattern needs to return a list of the names
;;   it introduces.
(struct class (name fields compile-pattern compile-template))

;; a class-field is a (class-field name class)
(struct class-field (name class))

;; literal-class is a really bad name since this is primarily used for
;; nonterminals
(define (literal-class name)
  (define (compile-pattern pattern source names rest)
    (define IR
      (ir:check-instance
       source name
       (ir:let (list (list pattern source))
               rest)))
    (list IR (cons pattern names)))

  (define (compile-template template dest names rest)
    (ir:let (list (list dest template))
            rest))

  (class name '() compile-pattern compile-template))

(module+ test
  (define literal1 (literal-class 'e))
  (check-equal?
   ((class-compile-pattern literal1) 'e_1 'self '() (ir:return '(e_1)))
   (list (ir:check-instance
          'self 'e
          (ir:let (list '(e_1 self))
                  (ir:return '(e_1))))
         '(e_1)))
  (check-equal?
   ((class-compile-template literal1) 'e 'result-e '(e) (ir:return '(result-e)))
   (ir:let (list '(result-e e))
           (ir:return '(result-e)))))

(define (sequence-class name fields)
  (define class-fields
    (for/list ([f fields]
               #:unless (symbol? (field-shape f)))
      (class-field (field-name f) (shape->class (field-shape f)))))

  ;; TODO do something about the flip-flopping order when pairing a
  ;; listof names with IR---make a struct or stick to one order
  ;; throughout the project
  (define (compile-pattern pattern source names rest)
    (define field-patterns
      (for/list ([p pattern]
                 [f fields]
                 #:unless (symbol? (field-shape f)))
        p))

    (define (compile-field pat f idx names+rest)
      (define compile-pat (class-compile-pattern (class-field-class f)))
      (define names (first names+rest))
      (define rest (second names+rest))

      (define projection-dest
        (if (symbol? pat)
            pat
            (format-symbol "~a-~a" source (class-field-name f))))

      (define fieldbody+names
        (compile-pat pat projection-dest (cons projection-dest names) rest))

      (list
       (second fieldbody+names)
       (ir:let (list (list projection-dest (ir:project name (class-field-name f) source)))
               (first fieldbody+names))))

    (define names+body
      (foldr
       compile-field
       (list names rest)
       field-patterns
       class-fields
       (build-list (length class-fields) identity)))

    (list
     (ir:check-instance
      source name
      (second names+body))
     (first names+body)))

  (define (compile-template template dest names rest)
    (define field-templates
      (for/list ([t template]
                 [f fields]
                 #:unless (symbol? (field-shape f)))
        t))
    (define subtemplate-dests
      (for/list ([t field-templates]
                 [f class-fields])
        (if (symbol? t)
            t
            (format-symbol "~a-~a" dest (class-field-name f)))))

    (define (compile-subtemplate t f dest rest)
      (define compile-t (class-compile-template (class-field-class f)))
      (compile-t t dest names rest))
    (foldr
     compile-subtemplate
     (ir:let (list (list dest (ir:make name subtemplate-dests)))
             rest)
     field-templates
     class-fields
     subtemplate-dests))

  (class name class-fields compile-pattern compile-template))

(module+ test
  (define filling-shape (choice 'filling '(veggies meat) #'here))
  (define sandwich-shape (sequence 'sandwich (list (field 'tag 'sandwich)
                                                   (field 'topslice 'bread)
                                                   (field 'filling filling-shape)
                                                   (field 'bottomslice 'bread))
                                   #'here))
  (define sandwichclass (shape->class sandwich-shape))

  ;; This test assumes we don't want to check literals that appear
  ;; inside of a sequence. I don't know if this is what we'll want in
  ;; the end, but it's an easier thing for now that works. It does
  ;; make me wonder what literals in their current state are useful
  ;; for other than for tags...
  (define sandwichpatcompiled+names
    ((class-compile-pattern sandwichclass) '(sandwich bread filling_1 bread)
    'self
    '()
    (ir:return '(filling_1))))
  (check-equal?
   (first sandwichpatcompiled+names)
   (ir:check-instance
    'self 'sandwich
    (ir:let (list (list 'filling_1 (ir:project 'sandwich 'filling 'self)))
            (ir:check-instance
             'filling_1 'filling
             (ir:let (list (list 'filling_1 'filling_1))
                     (ir:return '(filling_1)))))))
  (check-equal?
   (list->set (second sandwichpatcompiled+names))
   (list->set '(filling_1)))

  (check-equal?
   ((class-compile-template sandwichclass) '(sandwich bread veggies bread)
    'e-result
    '()
    (ir:return '(e-result)))
   ;; TODO Punning the name 'veggies with the datum 'veggies is going
   ;; to cause problems when we want to remove redundant let bindings.
   ;; One option may be to compile literals to a choice with 1 variant
   (ir:let (list (list 'veggies 'veggies))
           (ir:let (list (list 'e-result (ir:make 'sandwich '(veggies))))
                   (ir:return '(e-result)))))

  (define bag (sequence 'bag (list (field 'contents filling-shape)) #'here))
  (define bagofbags (sequence 'outerbag (list (field 'bag1 bag) (field 'bag2 bag)) #'here))
  (define bagofbagspatcompiled+names
    ((class-compile-pattern (shape->class bagofbags)) '((meat) (veggies))
     'self
     '()
     (ir:return '(meat veggies))))
  (check-equal?
   (first bagofbagspatcompiled+names)
   (ir:check-instance
    'self 'outerbag
    (ir:let (list (list 'self-bag1 (ir:project 'outerbag 'bag1 'self)))
            (ir:check-instance
             'self-bag1 'bag
             (ir:let (list (list 'meat (ir:project 'bag 'contents 'self-bag1)))
                     (ir:check-instance
                      'meat 'filling
                      (ir:let (list (list 'meat 'meat))
                              (ir:let (list (list 'self-bag2 (ir:project 'outerbag 'bag2 'self)))
                                      (ir:check-instance
                                       'self-bag2 'bag
                                       (ir:let (list (list 'veggies (ir:project 'bag 'contents 'self-bag2)))
                                               (ir:check-instance
                                                'veggies 'filling
                                                (ir:let (list (list 'veggies 'veggies))
                                                        (ir:return '(meat veggies))))))))))))))
  (check-equal?
   (list->set (second bagofbagspatcompiled+names))
   (list->set '(self-bag1 self-bag2 meat veggies)))

  (check-equal?
   ((class-compile-template (shape->class bagofbags)) '((meat) (meat))
    'e-result
    '()
    (ir:return '(e-result)))
   (ir:let
    (list (list 'meat 'meat))
    (ir:let
     (list (list 'e-result-bag1 (ir:make 'bag '(meat))))
     (ir:let
      (list (list 'meat 'meat))
      (ir:let
       (list (list 'e-result-bag2 (ir:make 'bag '(meat))))
       (ir:let
        (list (list 'e-result (ir:make 'outerbag '(e-result-bag1 e-result-bag2))))
        (ir:return '(e-result)))))))))

(define (shape->class shape)
  (cond
    [(symbol? shape) (literal-class shape)]
    [(choice? shape) (literal-class (choice-name shape))]
    [else
     (define name (sequence-name shape))
     (define fields (sequence-fields shape))
     (sequence-class name fields)]))

;; a method is a (method name class IR)
(struct method (name class body))

(define (steps->methods steps class-map c-shape e-shape k-shape)
  ;; step->methods : step -> (nonemptylistof method)
  (define (step->methods step)
    (syntax-parse (step-lhs step)
      [((~var c-pat (form-matching c-shape))
        (~var e-pat (form-matching e-shape))
        (~var k-pat (form-matching k-shape)))
       (define c-class (hash-ref class-map (attribute c-pat.name)))
       (define e-class (hash-ref class-map (attribute e-pat.name)))
       (define k-class (hash-ref class-map (attribute k-pat.name)))

       (define compile-c (class-compile-pattern c-class))
       (define compile-e (class-compile-pattern e-class))
       (define compile-k (class-compile-pattern k-class))

       (if (or (equal? (shape->name k-shape) (attribute k-pat.name))
               ;; TODO we don't yet emit things with this name but we
               ;; don't want to conflate matching on a shape for a
               ;; variant of k with a shape for a primitive.
               (equal? 'prim (attribute k-pat.name)))
           (error 'implement-typical-compile)
           (error 'implement-message-send-compile))]))
  (error 'implement-step-compilation))

(define (compile-cek name productions c-id e-id k-id steps)
  ;; For each of c, e, and k, we need to generate the appropriate
  ;; class hierarchy.
  (define shape-map (compile-grammar2 productions))
  (define-values (c-shape e-shape k-shape)
    (values
     (hash-ref shape-map (syntax-e c-id))
     (hash-ref shape-map (syntax-e e-id))
     (hash-ref shape-map (syntax-e k-id))))
  (define class-map (for/hash ([(name shape) (in-dict shape-map)])
                      (values name (shape->class shape))))

  (define methods (steps->methods steps class-map c-shape e-shape k-shape))

  ;; Although each shape will know its class, which itself will know
  ;; how to compile patterns that correspond to its shape, the shape
  ;; and class aren't in charge of when to compile k patterns as a
  ;; message send or not; that decision is made before we even hand a
  ;; pattern to the k class's compile-pattern function. The "driver"
  ;; that walks the c, e, and k registers and gets the classes/compile
  ;; functions looks at the k pattern and determines if it should be a
  ;; message send or not.
  ;; - If not, then it simply passes the pattern to the corresponding
  ;;   class' compile-pattern and adds the IR to the body of the
  ;;   control string's interpret method.
  ;; - If the k pattern should be compiled to a message send, the
  ;;   driver emits the IR for the message send---it knows what
  ;;   variables compiling the c and e patterns introduced, so it puts
  ;;   those in the right argument positions---and places the message
  ;;   send in the control string's interpret method. It then starts
  ;;   to emit IR for the k class' interpret method and, since it
  ;;   knows where it put the arguments for the message send, it can
  ;;   finish compiling the template for the body of the k class'
  ;;   interpret method.
  ;; This way, each class' compile function is treated uniformly. The
  ;; only non-uniform decisions are made exactly where we know how to
  ;; make that decision.

  ;; TODO primitives
  ;; TODO compiling steps into method bodies; collecting classes
  ;; TODO translating IR to RPython
  ;; TODO error messages (syntax-parse and runtime)

  #`(begin
      #,(compile-grammar name productions)
      ;; for now, I don't want to print out the values produced here
      (void (let () #,(compile-transition c-id e-id k-id steps)))))
