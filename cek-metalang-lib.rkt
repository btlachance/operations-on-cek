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

(module compile-util racket/base
  (require racket/match syntax/parse)
  (provide pattern-metavar)
  (define (matches-metavar? pattern id)
    (define without-suffix
      (match (symbol->string (syntax-e pattern))
        [(regexp #px"([^_]*)(_.+)?" (list _ contents suffix))
         (define symbol-without-suffix (string->symbol contents))
         (datum->syntax pattern symbol-without-suffix pattern)]))
    (free-identifier=? without-suffix id))

  (define-syntax-class (pattern-metavar id)
    #:description (format "metavar ~a" (syntax-e id))
    (pattern x:id
             #:when (matches-metavar? #'x id))))

(require
 (for-template 'keywords 'compile-util racket/pretty)
 (only-in 'keywords keywords-set make-keywords-delta-introducer)
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
;; - (ir:send name (listof name))
;; - (ir:return (listof name))
(struct ir:check-instance (arg class-name rest) #:transparent)
(struct ir:let (bindings rest) #:transparent)
(struct ir:send (receiver args) #:transparent)
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
;;                     (pattern source IR -> IR)
;;                     (template dest IR -> IR))
;; where compile-pattern assumes the pattern is an instance of the
;; class' shape; ditto for compile-template and templates.
;; Invariants:
;; - name must be equal? to the class' shape's name
(struct class (name fields compile-pattern compile-template) #:transparent)

;; a class-field is a (class-field name class)
(struct class-field (name class) #:transparent)

;; literal-class is a really bad name since this is primarily used for
;; nonterminals
(define (literal-class name)
  (define (compile-pattern pattern source rest)
    (ir:check-instance
     source name
     (ir:let (list (list pattern source))
             rest)))

  (define (compile-template template dest rest)
    (ir:let (list (list dest template))
            rest))

  (class name '() compile-pattern compile-template))

(module+ test
  (define literal1 (literal-class 'e))
  (check-equal?
   ((class-compile-pattern literal1) 'e_1 'self (ir:return '(e_1)))
   (ir:check-instance
    'self 'e
    (ir:let (list '(e_1 self))
            (ir:return '(e_1)))))
  (check-equal?
   ((class-compile-template literal1) 'e 'result-e (ir:return '(result-e)))
   (ir:let (list '(result-e e))
           (ir:return '(result-e)))))

(define (sequence-class name fields)
  (define class-fields
    (for/list ([f fields]
               #:unless (symbol? (field-shape f)))
      (class-field (field-name f) (shape->class (field-shape f)))))

  (define (compile-pattern pattern source rest)
    (define field-patterns
      (for/list ([p pattern]
                 [f fields]
                 #:unless (symbol? (field-shape f)))
        p))

    (define (compile-field pat f rest)
      (define compile-pat (class-compile-pattern (class-field-class f)))

      (define projection-dest
        (if (symbol? pat)
            pat
            (format-symbol "~a-~a" source (class-field-name f))))
      (ir:let (list (list projection-dest (ir:project name (class-field-name f) source)))
              (compile-pat pat projection-dest rest)))

    (ir:check-instance
     source name
     (foldr
      compile-field
      rest
      field-patterns
      class-fields)))

  (define (compile-template template dest rest)
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
      (compile-t t dest rest))
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
  (check-equal?
   ((class-compile-pattern sandwichclass) '(sandwich bread filling_1 bread)
    'self
    (ir:return '(filling_1)))
   (ir:check-instance
    'self 'sandwich
    (ir:let (list (list 'filling_1 (ir:project 'sandwich 'filling 'self)))
            (ir:check-instance
             'filling_1 'filling
             (ir:let (list (list 'filling_1 'filling_1))
                     (ir:return '(filling_1)))))))

  (check-equal?
   ((class-compile-template sandwichclass) '(sandwich bread veggies bread)
    'e-result
    (ir:return '(e-result)))
   ;; TODO Punning the name 'veggies with the datum 'veggies is going
   ;; to cause problems when we want to remove redundant let bindings.
   ;; One option may be to compile literals to a choice with 1 variant
   (ir:let (list (list 'veggies 'veggies))
           (ir:let (list (list 'e-result (ir:make 'sandwich '(veggies))))
                   (ir:return '(e-result)))))

  (define bag (sequence 'bag (list (field 'contents filling-shape)) #'here))
  (define bagofbags (sequence 'outerbag (list (field 'bag1 bag) (field 'bag2 bag)) #'here))
  (check-equal?
   ((class-compile-pattern (shape->class bagofbags)) '((meat) (veggies))
    'self
    (ir:return '(meat veggies)))
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
   ((class-compile-template (shape->class bagofbags)) '((meat) (meat))
    'e-result
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

;; a method is a (method class (listof name) IR)
;; representing the interpret method of the specified class
(struct method (class args body) #:transparent)

(define (compile-rhs c-shape e-shape k-shape class-map rhs)
  (syntax-parse rhs
    [((~var c-temp (form-matching c-shape))
      (~var e-temp (form-matching e-shape))
      (~var k-temp (form-matching k-shape)))
     (define compile-c-temp (class-compile-template (hash-ref class-map (attribute c-temp.name))))
     (define compile-e-temp (class-compile-template (hash-ref class-map (attribute e-temp.name))))
     (define compile-k-temp (class-compile-template (hash-ref class-map (attribute k-temp.name))))

     (define-values (c-dest e-dest k-dest)
       (values 'c* 'e* 'k))
     (compile-c-temp
      (syntax->datum #'c-temp) c-dest
      (compile-e-temp
       (syntax->datum #'e-temp) e-dest
       (compile-k-temp
        (syntax->datum #'k-temp) k-dest
        (ir:return (list c-dest e-dest k-dest)))))]))

(define (steps->methods steps class-map c-shape e-shape k-shape)
  (define (step->methods step)
    (syntax-parse (step-lhs step)
      [((~var c-pat (form-matching c-shape))
        (~var e-pat (form-matching e-shape))
        (~var k-pat (form-matching k-shape)))
       (define c-class (hash-ref class-map (attribute c-pat.name)))
       (define e-class (hash-ref class-map (attribute e-pat.name)))
       (define k-class (hash-ref class-map (attribute k-pat.name)))

       (define compile-c-pat (class-compile-pattern c-class))
       (define compile-e-pat (class-compile-pattern e-class))
       (define compile-k-pat (class-compile-pattern k-class))

       (define-values (c-src e-src k-src)
         (values 'self 'env 'k))

       (if (or (equal? (shape->name k-shape) (attribute k-pat.name))
               ;; TODO we don't yet emit things with this name but we
               ;; don't want to conflate matching on a shape for a
               ;; variant of k with a shape for a primitive.
               (equal? 'prim (attribute k-pat.name)))
           (list
            (method
             c-class
             (list c-src e-src k-src)
             (compile-c-pat
              (syntax->datum #'c-pat) c-src
              (compile-e-pat
               (syntax->datum #'e-pat) e-src
               (compile-k-pat
                (syntax->datum #'k-pat) k-src
                (compile-rhs c-shape e-shape k-shape class-map (step-rhs step)))))))
           (list
            (method
             c-class
             (list c-src e-src k-src)
             (compile-c-pat
              (syntax->datum #'c-pat) c-src
              (compile-e-pat
               (syntax->datum #'e-pat) e-src
               ;; TODO can we do better than a gensym? I would like
               ;; something testable without resorting to alpha
               ;; equivalence
               (let ([k (gensym 'k)])
                 ;; here we just check that our third argument is
                 ;; actually a k and then send the control string and
                 ;; environment to it
                 ((class-compile-pattern (hash-ref class-map (shape->name k-shape)))
                  k k-src
                  ;; the order of these arguments is important and
                  ;; must match the order of the args in the method on
                  ;; k-class below
                  (ir:send k (list (syntax->datum #'c-pat) (syntax->datum #'e-pat))))))))
            (let ()
              (define-values (k-src-cont c-src-cont e-src-cont)
                (values 'self 'e-arg 'env-arg))
              (method
               k-class
               (list k-src-cont c-src-cont e-src-cont)
               (compile-k-pat
                (syntax->datum #'k-pat) k-src-cont
                (compile-c-pat
                 (syntax->datum #'c-pat) c-src-cont
                 (compile-e-pat
                  (syntax->datum #'e-pat) e-src-cont
                  (compile-rhs c-shape e-shape k-shape class-map (step-rhs step)))))))))]))
  (apply append (map step->methods steps)))

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
  (pretty-print methods)

  ;; TODO primitives
  ;; TODO collecting classes
  ;; TODO translating IR to RPython
  ;; TODO error messages (syntax-parse and runtime)

  #'(begin))
