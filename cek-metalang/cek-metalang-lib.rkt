#lang racket
(require
 racket/syntax
 syntax/parse
 syntax/id-set
 "ir.rkt"
 "py-from-ir.rkt"
 "compile-util.rkt"
 (for-template
  racket/base
  syntax/parse
  syntax/id-set
  "keywords.rkt"))

(provide production step compile-cek)
(module+ test
  (require rackunit))

(struct production (name forms)
  #:name -production
  #:constructor-name -production)
(define-syntax-class production
  #:attributes (name (form 1) data)
  #:literals (::=)
  (pattern (name:id ::= form ...+)
           #:attr data (-production #'name (attribute form))))

;; a step is a (-step syntax syntax (listof (list syntax syntax)))
(struct step (lhs rhs wheres)
  #:name -step
  #:constructor-name -step)
(define-splicing-syntax-class where-clause
  #:attributes (body)
  (pattern (~seq #:where pattern template)
           #:attr body (list #'pattern #'template)))
(define-syntax-class step
  #:attributes (lhs rhs (wheres 1) data)
  #:literals (-->)
  (pattern [(~and lhs (e-l env-l k-l))
            -->
            (~and rhs (e-r env-r k-r))
            wheres:where-clause ...]
           #:attr data (-step #'lhs #'rhs (attribute wheres.body))))

;; a name is a symbol

;; a shape is one of
;; - symbol, representing a literal
;; - (sequence name (listof field) stx)
;; - (choice name (U #f (nonemptylistof shape)) id)
;;   * a choice's alternatives is only #f while parsing the grammar;
;;     once it is set to a non-#f value it is then only changed by
;;     the prim-creating code
;;   * TODO I probably also want a restriction on what's in the list
;;   of alternatives. For example, I don't want a choice be able to
;;   produce itself without going through a sequence and I don't want
;;   a choice's name to be in its list of alternatives either. The
;;   latter couldn't occur during grammar generation, as the name
;;   would always be resolved to the choice, but it's an odd
;;   possibility in the data structure choice.
;; - (prim name (sexp -> bool) class)
(struct sequence (name fields stx))
(struct choice (name [alternatives #:mutable] nonterminal))
(struct prim (name matches? class))

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
    [(choice? shape) (choice-name shape)]
    [else (prim-name shape)]))

(require "compile-util.rkt")

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
                                 (~bind [name (attribute ss.name)]))))
                (~and (~fail #:unless (prim? shape))
                      _
                      (~fail #:unless ((prim-matches? shape) (syntax->datum this-syntax)))
                      (~bind [name (prim-name shape)])))))

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

  (define nat-prim (prim 'nat natural-number/c #f))
  (define nat?
    (syntax-parser
      [(~var _ (form-matching nat-prim)) #t]
      [_ #f]))
  (check-true (nat? #'5))
  (check-false (nat? #'-10))
  (check-false (nat? #'"hello"))
  (check-equal?
   (syntax-parse #'5
     [(~var five (form-matching nat-prim)) (attribute five.name)])
   'nat)

  (define (matches-lookup? sexp)
    (match sexp
      [`(lookup ,_ ,_) #t]
      [_ #f]))
  (check-true (matches-lookup? '(lookup x env)))
  (define lookup-prim (prim 'lookup matches-lookup? #f))
  (define lookup?
    (syntax-parser
      [(~var _ (form-matching lookup-prim)) #t]
      [_ #f]))
  (check-true (lookup? #'(lookup x env)))

  (define combination-of-prims?
    (syntax-parser
      [(~var _ (one-of (list nat-prim lookup-prim))) #t]
      [_ #f]))
  (check-true (combination-of-prims? #'5))
  (check-true (combination-of-prims? #'(lookup x e)))
  (check-false (combination-of-prims? #'(lookup)))
  (check-false (combination-of-prims? #'-10))

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
       (define comb-name (format-symbol "~a_comb~a" p-name i))
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
;; INV: productions are sorted s.t. the production defining a
;; nonterminal that is an alternative for some other nonterminal comes
;; after that other nonterminal's production
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


;; a class is a (class name
;;                     (U (listof class-field) #f)
;;                     (pattern name IR -> IR)
;;                     (template name IR -> IR))
;; - when fields is #f the class represents a literal, i.e. it does
;;   not have a constructor
;; - when super-name is #f the class is only relevant for compile-time
;;   and should not have a residual in the generated code
;; Invariants:
;; - compile-pattern assumes the pattern is an instance of the
;;   class' shape; ditto for compile-template and templates.
;; - name must be equal? to the class' shape's name
(struct class (name fields compile-pattern compile-template) #:transparent)

;; a class-field is a (class-field name class)
(struct class-field (name class) #:transparent)

;; nonterminal-class : name (nelistof shape) (map name class) -> class
(define (nonterminal-class name alternatives class-map)
  (define (compile-pattern pattern source rest)
    (syntax-parse pattern
      [(~var p (one-of alternatives))
       (define compile-p (class-compile-pattern (hash-ref class-map (attribute p.name))))
       (compile-p pattern source rest)]
      [_
       (ir:check-instance
        source name
        (ir:let (list (list pattern source))
                rest))]))

  (define (compile-template template dest rest)
    (syntax-parse template
      [(~var t (one-of alternatives))
       (define compile-t (class-compile-template (hash-ref class-map (attribute t.name))))
       (compile-t template dest rest)]
      [_
       (ir:let (list (list dest template))
               rest)]))
  (class name '() compile-pattern compile-template))

(module+ test
  (define nt1 (nonterminal-class 'e '(x y z) (make-hash)))
  (check-equal?
   ((class-compile-pattern nt1) 'e_1 'self (ir:return '(e_1)))
   (ir:check-instance
    'self 'e
    (ir:let (list '(e_1 self))
            (ir:return '(e_1)))))
  (check-equal?
   ((class-compile-template nt1) 'e 'result-e (ir:return '(result-e)))
   (ir:let (list '(result-e e))
           (ir:return '(result-e)))))

;; sequence-class : name (listof field) (map name class) -> class
;; INV: class-map must have bindings for each non-symbol field in
;; fields
(define (sequence-class name fields class-map)
  ;; The meaning of a combination in the pattern/template language is
  ;; more interesting than nonterminals. In a pattern, a combination
  ;; deconstructs the source and introduces new bindings; in a
  ;; template, it constructs a new value out of existing bindings and
  ;; puts that value into dest.
  (define class-fields
    (for/list ([f fields]
               #:unless (symbol? (field-shape f)))
      (class-field (field-name f) (hash-ref class-map (shape->name (field-shape f))))))

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
            (format-symbol "~a_~a" source (class-field-name f))))
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
            (format-symbol "~a_~a" dest (class-field-name f)))))

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
  (define sandwiches-shape-map (hash 'filling filling-shape
                                     'veggies 'veggies
                                     'meat 'meat))
  (define sandwiches-class-map (make-hash))
  (build-classes! sandwiches-class-map (make-hash) sandwiches-shape-map)
  (define sandwich-shape (sequence 'sandwich (list (field 'tag 'sandwich)
                                                   (field 'topslice 'bread)
                                                   (field 'filling filling-shape)
                                                   (field 'bottomslice 'bread))
                                   #'here))
  (define sandwichclass (shape->class sandwich-shape sandwiches-class-map))

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
   (ir:let (list (list 'veggies (ir:make 'veggies #f)))
           (ir:let (list (list 'e-result (ir:make 'sandwich '(veggies))))
                   (ir:return '(e-result)))))


  (define bag (sequence 'bag (list (field 'contents filling-shape)) #'here))
  (define bagofbags (sequence 'outerbag (list (field 'bag1 bag) (field 'bag2 bag)) #'here))
  (define bags-shape-map (hash-set* sandwiches-shape-map
                                    'bag bag
                                    'outerbag bagofbags))
  (define bags-class-map (hash-copy sandwiches-class-map))
  (build-classes! bags-class-map (make-hash) bags-shape-map)

  (check-equal?
   ((class-compile-pattern (shape->class bagofbags bags-class-map)) '((meat) (veggies))
    'self
    (ir:return '(meat veggies)))
   (ir:check-instance
    'self 'outerbag
    (ir:let (list (list 'self_bag1 (ir:project 'outerbag 'bag1 'self)))
            (ir:check-instance
             'self_bag1 'bag
             (ir:let (list (list 'meat (ir:project 'bag 'contents 'self_bag1)))
                     (ir:check-instance
                      'meat 'meat
                      (ir:let (list (list 'meat 'meat))
                              (ir:let (list (list 'self_bag2 (ir:project 'outerbag 'bag2 'self)))
                                      (ir:check-instance
                                       'self_bag2 'bag
                                       (ir:let (list (list 'veggies (ir:project 'bag 'contents 'self_bag2)))
                                               (ir:check-instance
                                                'veggies 'veggies
                                                (ir:let (list (list 'veggies 'veggies))
                                                        (ir:return '(meat veggies))))))))))))))

  (check-equal?
   ((class-compile-template (shape->class bagofbags bags-class-map)) '((meat) (meat))
    'e-result
    (ir:return '(e-result)))
   (ir:let
    (list (list 'meat (ir:make 'meat #f)))
    (ir:let
     (list (list 'e-result_bag1 (ir:make 'bag '(meat))))
     (ir:let
      (list (list 'meat (ir:make 'meat #f)))
      (ir:let
       (list (list 'e-result_bag2 (ir:make 'bag '(meat))))
       (ir:let
        (list (list 'e-result (ir:make 'outerbag '(e-result_bag1 e-result_bag2))))
        (ir:return '(e-result)))))))))

;; symbol-class : name -> class
(define (symbol-class symbol)
  (define (compile-pattern pattern source rest)
    (ir:check-instance
     source symbol
     (ir:let (list (list pattern source))
             rest)))
  (define (compile-template template dest rest)
    ;; TODO Depending on what variables are bound in surrounding
    ;; patterns, template could be referring to a bound variable. If
    ;; it is refering to such a variable, then it seems a little odd
    ;; that we're constructing an instance of symbol here. Remember,
    ;; though, that this is the template for a class with no
    ;; fields. We now have to decide whether two "instances" of a
    ;; class can be distinguished. If they can, then it's incorrect to
    ;; use ir:make here universally---if template refers to a bound
    ;; variable then constructing a new instance of symbol would
    ;; produce a value that isn't equivalent to what's in the bound
    ;; variable. On the other hand, if template does not refer to a
    ;; bound variable, then constructing a new instance would be
    ;; correct. So, if two instances of a symbol-class can be
    ;; distinguished, then we have to know whether or not template
    ;; refers to a bound variable.
    (ir:let (list (list dest (ir:make symbol #f)))
            rest))
  (class symbol #f compile-pattern compile-template))

;; shape->class : shape (map name class) -> class
;; INV: when shape is a sequence, class-map must have bindings for
;; each of its non-symbol fields
(define (shape->class shape class-map)
  (cond
    [(symbol? shape) (symbol-class shape)]
    [(sequence? shape) (sequence-class (sequence-name shape)
                                       (sequence-fields shape)
                                       class-map)]
    [(choice? shape) (nonterminal-class (choice-name shape)
                                        (choice-alternatives shape)
                                        class-map)]
    [else (prim-class shape)]))

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
       (values 'c_result 'e_result 'k_result))
     (compile-c-temp
      (syntax->datum #'c-temp) c-dest
      (compile-e-temp
       (syntax->datum #'e-temp) e-dest
       (compile-k-temp
        (syntax->datum #'k-temp) k-dest
        (ir:return (list c-dest e-dest k-dest)))))]))

(define (steps->methods steps class-map toplevel-shape c-shape e-shape k-shape)
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

       (define-syntax-class top
         #:attributes (name)
         (pattern (~var p (form-matching toplevel-shape))
                  #:attr name (attribute p.name)))
       (define (compile-wheres wheres rest)
         ;; Compiling a where clause requires compiling the template
         ;; and then compiling the pattern: the template produces
         ;; things that the pattern consumes, and so they must be
         ;; compiled in that order. (The reason the rest of the system
         ;; is setup for compiling patterns before templates is
         ;; because those patterns consume function arguments, which
         ;; are already produced, and those templates produce things
         ;; that get consumed by the function's return)
         (define (compile-where w idx r)
           (syntax-parse w
             [(pat:top templ:top)
              (define compile-pat (class-compile-pattern (hash-ref class-map (attribute pat.name))))
              (define compile-templ (class-compile-template (hash-ref class-map (attribute templ.name))))
              (compile-templ
               (syntax->datum #'templ) (format-symbol "w_tmp~a" idx)
               (compile-pat
                (syntax->datum #'pat) (format-symbol "w_tmp~a" idx)
                r))]))
         (foldr
          compile-where
          rest
          (or wheres '())
          (build-list (length (or wheres '())) values)))

       (define-values (c-src e-src k-src)
         (values 'self 'e 'k))

       (if (or (equal? (shape->name k-shape) (attribute k-pat.name))
               ;; There are three cases: either the pattern is the k
               ;; metavariable, a primitive pattern, or a pattern for
               ;; some other part of the grammar. We only know we're
               ;; in the first case if we rule out the other two.
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
                (compile-wheres
                 (step-wheres step)
                 (compile-rhs c-shape e-shape k-shape class-map (step-rhs step))))))))
           (list
            (method
             c-class
             (list c-src e-src k-src)
             (compile-c-pat
              (syntax->datum #'c-pat) c-src
              (compile-e-pat
               (syntax->datum #'e-pat) e-src
               (let ([k 'dispatch_k])
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
                (values 'self 'c_arg 'e_arg))
              (method
               k-class
               (list k-src-cont c-src-cont e-src-cont)
               (compile-k-pat
                (syntax->datum #'k-pat) k-src-cont
                (compile-c-pat
                 (syntax->datum #'c-pat) c-src-cont
                 (compile-e-pat
                  (syntax->datum #'e-pat) e-src-cont
                  (compile-wheres
                   (step-wheres step)
                   (compile-rhs c-shape e-shape k-shape class-map (step-rhs step))))))))))]))
  (apply append (map step->methods steps)))

;; add-prims! : class shape shape -> (listof shape)
;; given
;;  - a class that can compile any pattern/template that the user wrote
;;  - choice shapes for the control string and the environment
;; this function mutates the alternatives in c-shape and e-shape with
;; prim shapes that produce control strings and environments and
;; returns the added prims
(define (add-prims! toplevel-class c-shape e-shape)
  (define (compile-subtemplate t f dest rest)
    (define compile-t (class-compile-template (class-field-class f)))
    (compile-t t dest rest))
  (define prim-lookup
    (let ()
      (define lookup-fields (list (class-field 'env toplevel-class)
                                  (class-field 'var toplevel-class)))
      (define (compile-lookup-template template dest rest)
        (define subtemplates (cdr template))
        (define dests
          (for/list ([f lookup-fields])
            (format-symbol "~a_~a" dest (class-field-name f))))
        (foldr
         compile-subtemplate
         (ir:let (list (list dest (ir:call-builtin 'lookup dests)))
                 rest)
         subtemplates
         lookup-fields
         dests))
      (prim
       'lookup
       (match-lambda [`(lookup ,_ ,_) #t] [_ #f])
       (class
         'lookup
         lookup-fields
         (lambda (pattern source rest)
           (error 'compile-pattern "Cannot compile lookup to a pattern"))
         compile-lookup-template))))
  (define prim-extend
    (let ()
      (define extend-fields (list (class-field 'env toplevel-class)
                                  (class-field 'var toplevel-class)
                                  (class-field 'val toplevel-class)))
      (define (compile-extend-template template dest rest)
        (define subtemplates (cdr template))
        (define dests
          (for/list ([f extend-fields])
            (format-symbol "~a_~a" dest (class-field-name f))))
        (foldr
         compile-subtemplate
         (ir:let (list (list dest (ir:call-builtin 'extend dests)))
                 rest)
         subtemplates
         extend-fields
         dests))
      (prim
       'extend
       (match-lambda [`(extend ,_ ,_ ,_) #t] [_ #f])
       (class
         'extend
         extend-fields
         (lambda (pattern source rest)
           (error 'compile-pattern "Cannot compile extend to a pattern"))
         compile-extend-template))))

  (set-choice-alternatives!
   c-shape
   (cons
    prim-lookup
    (choice-alternatives c-shape)))
  (set-choice-alternatives!
   e-shape
   (cons
    prim-extend
    (choice-alternatives e-shape)))
  (list prim-lookup prim-extend))

;; a super-class is one of
;; - #f
;; - 'top
;; - name, where name != 'top
;; A class whose super-class is #f is only relevant for compile time
;; and should have no residual in the generated code

;; build-classes! : (map name class) (map name super-class) (map name shape) -> void
;; mutates class-map and super-class-map so that every shape in
;; shape-map has a class/super-class entry
(define (build-classes! class-map super-class-map shape-map)
  (define choice-shapes (filter choice? (hash-values shape-map)))
  ;; Unless we make the classes mutable, we need to use a mutable hash
  ;; map to build the class-map (in a similar way to how we build up
  ;; shape-map). Assuming we go with the latter, create a class for
  ;; each choice shape and add it to the map---it's OK that we don't
  ;; yet have classes for each of the choice's alternatives, since the
  ;; choice only uses those when its compile-pattern/template is
  ;; called, which only happens after the class map is fully
  ;; constructed. Then, for each remaining shape, create its
  ;; class---if it needs to look something up in the class-map, we
  ;; know that that something must be choice and all choice shapes
  ;; have a class in class-map
  (for ([c choice-shapes])
    (hash-set! class-map (choice-name c) (shape->class c class-map))
    (for ([alt (choice-alternatives c)])
      (hash-set! super-class-map (shape->name alt) (choice-name c))))

  (for ([c choice-shapes]
        #:unless (hash-has-key? super-class-map (choice-name c)))
    (hash-set! super-class-map (choice-name c) 'top))

  (define remaining-shapes (filter-not choice? (hash-values shape-map)))
  (for ([s remaining-shapes])
    (hash-set! class-map (shape->name s) (shape->class s class-map))))

;; toplevel-class : (listof shape) (map name class) -> class
;; makes a class like nonterminal-class but errors if the
;; pattern/template does not match any of choice-shapes
(define (toplevel-class choice-shapes class-map)
  (define (compile-pattern pattern source rest)
    (syntax-parse pattern
      [(~var p (one-of choice-shapes))
       (define compile-p (class-compile-pattern (hash-ref class-map (attribute p.name))))
       (compile-p pattern source rest)]))
  (define (compile-template template dest rest)
    (syntax-parse template
      [(~var t (one-of choice-shapes))
       (define compile-t (class-compile-template (hash-ref class-map (attribute t.name))))
       (compile-t template dest rest)]))

  (class 'toplevel '() compile-pattern compile-template))

(define (compile-cek name productions c-id e-id k-id steps)
  (define shape-map (compile-grammar2 productions))
  (define-values (c-shape e-shape k-shape)
    (values
     (hash-ref shape-map (syntax-e c-id))
     (hash-ref shape-map (syntax-e e-id))
     (hash-ref shape-map (syntax-e k-id))))

  ;; Tying the knot for prims has felt like a really big hack... all
  ;; of this mutation wasn't thought through, which makes it feel like
  ;; more of a hack
  (define class-map (make-hash))
  (define super-class-map (make-hash))

  (define choice-shapes (filter choice? (hash-values shape-map)))
  (define toplevel-s (choice 'toplevel choice-shapes #'here))
  ;; because shape-map is immutable we don't add prims to it; thus, to
  ;; get their super class entries into super-class-map, we have to do
  ;; that outside of build-classes
  (define toplevel-c (toplevel-class choice-shapes class-map))
  (define prims (add-prims! toplevel-c c-shape e-shape))
  (build-classes! class-map super-class-map shape-map)
  (for ([p prims])
    (hash-set! super-class-map (prim-name p) #f)
    (hash-set! class-map (prim-name p) (shape->class p class-map)))
  (hash-set! super-class-map 'toplevel #f)

  (define methods (steps->methods steps class-map toplevel-s c-shape e-shape k-shape))
  (define method-map
    (for/fold ([method-map (hash)])
              ([m methods])
      (define c (method-class m))
      (define existing-method (hash-ref method-map (class-name c) #f))
      (when (and existing-method
                 (not (equal? (method-body existing-method) (method-body m)))
                 (not (equal? (method-args existing-method) (method-args m))))
        (error 'compile-cek "class ~a has two methods that are not equivalent; method 1: ~a\n method 2: ~a"
               (method-body existing-method)
               (method-body m)))
      (hash-set method-map (class-name c) m)))

  (define (class-field->field-def c-f)
    (ir:field-def (class-field-name c-f) (class-name (class-field-class c-f))))
  (define (method->method-def m)
    (ir:method-def
     (method-args m)
     (method-body m)))

  ;; check-for-super-method: name super-class -> (U ir:method-def 'super)
  ;; assumes that super-name is non-#f and that either it is 'top or
  ;; it has a non-#f super-class according to super-class-map
  (define (check-for-super-method class-name super-name)
    (let loop ([defining-class super-name])
      (match defining-class
        ['top
         (ir:method-def
          '()
          (ir:error (format "class ~a does not implement a method" class-name)))]
        [_
         (if (hash-has-key? method-map defining-class)
             'super
             (loop (hash-ref super-class-map defining-class)))])))
  
  (define class-definitions
    (for/hash ([class-name (in-hash-keys class-map)]
               #:when (hash-ref super-class-map class-name))
      (define def
        (ir:class-def
         class-name
         (hash-ref super-class-map class-name)
         (match (class-fields (hash-ref class-map class-name))
           [(list fields ...) (map class-field->field-def fields)]
           [_ #f])
         (if (hash-has-key? method-map class-name)
             (method->method-def (hash-ref method-map class-name))
             (check-for-super-method class-name (hash-ref super-class-map class-name)))))
      (values class-name def)))

  ;; in python, super class declarations must come before a subclass'
  ;; declaration... given that all of the nonterminals are the only
  ;; superclasses, it's sufficient to print those out first (and thus
  ;; I can put off implementing a topological sort even longer)
  (define root-names (map (compose syntax-e production-name) productions))
  (for ([root root-names])
    (pretty-display (class-def->py (hash-ref class-definitions root))))
  (for ([remaining (remove* root-names (hash-keys class-definitions))])
    (pretty-display (class-def->py (hash-ref class-definitions remaining)) #:newline? #t))

  ;; TODO error messages (syntax-parse and runtime)
  #'(begin))
