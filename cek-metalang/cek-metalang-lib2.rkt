#lang racket
(require syntax/parse)

;; a sort is one of
;; - symbol, representing a terminal
;; - (listof (U nonterminal symbol))

;; a nonterminal is a (nt symbol)
(struct nt (symbol) #:transparent)

;; a suffix is one of
;; - natural
;; - symbol

;; a ast is one of
;; - symbol, representing a terminal
;; - (metavar symbol (U #f suffix), where nt is the symbol for some
;;   nonterminal. if the suffix is non-#f then it represents the
;;   suffix in a metavariable, e.g. the 0 in e_0
;; - (metafunction symbol (listof ast) sort)
;;   binding bound to ...? (something that knows how to compile it)
;; - (prim any/c id) where the id has a transformer binding bound to
;;   a prim-info
;; - (compound (listof ast) sort), representing a non-atomic form like cons
(struct metavar (nt suffix) #:transparent)
(struct metafunction (name args sort) #:transparent)
(struct prim (data id) #:transparent)
(struct compound (asts sort) #:transparent)

;; a binding is a (binding metavar type)
(struct binding (metavar type))

;; lookup-ty : metavar (listof binding) -> (U type #f)
(define (lookup-ty metavar bindings)
  (define (find-metavar bs)
    (match bs
      [(list b bs* ...)
       (if (equal? (binding-metavar b) metavar)
           (binding-type b)
           (find-metavar bs*))]
      [_ #f]))
  (find-metavar bindings))

;; a tc-template-result is one of
;; - #f
;; - (tc-template-result type)
(struct tc-template-result (type))

(struct tc-pattern-result (type bindings))

;; Typechecking a pattern must produce a type. I had this in the model
;; but I don't know why I didn't translate it in the implementation

;; TODO instances of the lang struct are generated from the grammar
;; Somewhere (possibly in the lang) we also need to know which forms
;; get compiled down to class-def's

;; a lang is a (lang (symbol -> type)
;;                   (symbol -> type)
;;                   (sort (listof ast) -> tc-pattern-result
;;                   (sort (listof ast) (listof binding) -> type)
;;                   (pattern source rest -> IR)
;;                   (template dest rest -> IR))
(struct lang (nt->type
              literal->type
              tc-compound-pattern
              tc-compound-template
              compile-pattern
              compile-template))

;; a prim-info is a (prim-info (any/c -> tc-pattern-result)
;;                             (any/c (listof binding) -> type)
;;                             (pattern source rest -> IR)
;;                             (template dest rest -> IR))
(struct prim-info (tc-pattern
                   tc-template
                   compile-pattern
                   compile-template))

;; a parse-fun is a (syntax -> (U ast #f))
;; a parser is a (parser parse-fun parse-fun)
(struct parser (parse-template parse-pattern))

;; a prim-parser is like a parser but the parse-fun's have the type
;; (syntax lang -> (U prim #f))

;; symbol->metavar : nt symbol -> (U metavar #f)
(define (symbol->metavar expected sym)
  (match (symbol->string sym)
    [(regexp #px"^([^_]+)(_([^_]+))?$" (list _ nt _ suffix))
     #:when (equal? (symbol->string (nt-symbol expected)) nt)
     (metavar (string->symbol nt) (and suffix
                                       (or
                                        ;; TODO check suffix is a nat
                                        (string->number suffix 10)
                                        (string->symbol suffix))))]
    [_ #f]))
(define-syntax-class (-metavar-of nt)
  #:attributes (data)
  (pattern x:id
           #:attr data (symbol->metavar nt (syntax-e #'x))
           #:when (metavar? (attribute data))))
(define-syntax-class (-metavar nts)
  #:attributes (data)
  (pattern (~and (~fail #:when (empty? nts))
                 (~or (~var mv1 (-metavar-of (car nts)))
                      (~var mv2 (-metavar (cdr nts))))
                 (~bind [data (or (attribute mv1.data) (attribute mv2.data))]))))

(define-syntax-class (-terminal-of terminal)
  #:attributes (data)
  (pattern :id
           #:when (equal? (syntax-e this-syntax) terminal)
           #:attr data terminal))
(define-syntax-class (-terminal terminals)
  #:attributes (data)
  (pattern (~and (~fail #:when (empty? terminals))
                 (~or (~var t1 (-terminal-of (car terminals)))
                      (~var t2 (-terminal (cdr terminals))))
                 (~bind [data (or (attribute t1.data) (attribute t2.data))]))))

(define-syntax-class (-metafunction-of sort parser)
  #:attributes (data)
  (pattern (name args ...)
           #:when (equal? (syntax-e #'name) (car sort))
           #:when (= (length (attribute args)) (length (cdr sort)))
           #:attr data (metafunction (syntax-e #'name) (map parser (attribute args)) sort)))
(define-syntax-class (-metafunction sorts parser)
  #:attributes (data)
  (pattern (~and (~fail #:when (empty? sorts))
                 (~or (~var mf1 (-metafunction-of (car sorts) parser))
                      (~var mf2 (-metafunction (cdr sorts) parser)))
                 (~bind [data (or (attribute mf1.data) (attribute mf2.data))]))))

(define-syntax-class (-prim-of prim-parse-fun)
  #:attributes (data)
  (pattern _
           #:attr data (prim-parse-fun this-syntax)
           #:when (prim? (attribute data))))
(define-syntax-class (-prim prim-parse-funs)
  #:attributes (data)
  (pattern (~and (~fail #:when (empty? prim-parse-funs))
                 (~or (~var prim1 (-prim-of (car prim-parse-funs)))
                      (~var prim2 (-prim (cdr prim-parse-funs))))
                 (~bind [data (or (attribute prim1.data) (attribute prim2.data))]))))

(define-syntax-class (-sub-of atomic-sort parse-fun)
  #:attributes (data)
  (pattern id
           #:when (symbol? atomic-sort)
           #:when (equal? (syntax-e this-syntax) atomic-sort)
           #:attr data atomic-sort)
  (pattern _
           #:when (nt? atomic-sort)
           #:attr data (parse-fun this-syntax)
           #:when (attribute data)))
(define-syntax-class (-compound-of list-sort parse-fun)
  #:attributes (data)
  (pattern ()
           #:when (empty? list-sort)
           #:attr data (compound '() '()))
  (pattern (~and (~fail #:when (empty? list-sort))
                 ((~var hd (-sub-of (car list-sort) parse-fun))
                  .
                  (~var tl (-compound-of (cdr list-sort) parse-fun))))
           #:attr data (compound
                        (append
                         (if (nt? (car list-sort))
                             (list (attribute hd.data))
                             '())
                         (compound-asts (attribute tl.data)))
                        list-sort)))
(define-syntax-class (-compound sorts parse-fun)
  #:attributes (data)
  (pattern (~and (~fail #:when (empty? sorts))
                 (~or (~var c1 (-compound-of (car sorts) parse-fun))
                      (~var c2 (-compound (cdr sorts) parse-fun)))
                 (~bind [data (or (attribute c1.data) (attribute c2.data))]))))

;; lang-parser : (setof symbol) (setof nonterminal) (setof sort)
;;               (setof sort) (listof prim-parser) -> parser
;; invariants to ensure that terminals parse as terminals, metavars
;; parse as metavars, and that things otherwise work correctly:
;;  - the symbols in terminals and nonterminals do not overlap
;;  - all sorts in metafunctions are non-symbol sorts, where each
;;    such sort has a symbol as a first element and that symbol does
;;    not overlap with any of the non/terminals
(define (lang-parser terminals nonterminals compounds
                     metafunctions prim-parsers)
  (define (parse-temp stx)
    (syntax-parse stx
      [(~var t (-terminal terminals))
       (attribute t.data)]
      [(~var mv (-metavar nonterminals))
       (attribute mv.data)]
      [(~var mf (-metafunction metafunctions parse-temp))
       (attribute mf.data)]
      [(~var prim (-prim (map parser-parse-template prim-parsers)))
       (attribute prim.data)]
      [(~and (subtemps ...) (~var c (-compound compounds parse-temp)))
       (attribute c.data)]))
  (define (parse-pat stx)
    (syntax-parse stx
      [(~var t (-terminal terminals))
       (attribute t.data)]
      [(~var mv (-metavar nonterminals))
       (attribute mv.data)]
      [(~var prim (-prim (map parser-parse-pattern prim-parsers)))
       (attribute prim.data)]
      [(~and (subpats ...) (~var c (-compound compounds parse-pat)))
       (attribute c.data)]))
  (parser parse-temp parse-pat))

(module+ test
  (require rackunit)
  ;; TODO So that all metavariables are parsed in one place, by the
  ;; -metavar class, primitives should communicate to the
  ;; using-language their desired nonterminal(s).
  (define var-parse-fun
    ;; TODO What if the prim has subtemplates/subpatterns that need
    ;; parsing?
    (syntax-parser
      [:id
       (prim (syntax-e this-syntax) #'var)]
      [_ #f]))
  (define var-parser (parser var-parse-fun var-parse-fun))
  (define t1-parser (lang-parser (list 'mt)
                                 (list (nt 'e) (nt 'x))
                                 (list (list 'lambda (nt 'x) (nt 'e))
                                       (list (nt 'e) (nt 'e)))
                                 ;; (pick e e)
                                 (list (list 'pick (nt 'e) (nt 'e)))
                                 (list var-parser)))
  (define t1-parse-t (parser-parse-template t1-parser))
  (define t1-parse-p (parser-parse-pattern t1-parser))

  (check-equal? (t1-parse-t #'(lambda x x))
                (compound (list (metavar 'x #f) (metavar 'x #f))
                          (list 'lambda (nt 'x) (nt 'e))))
  (check-equal? (t1-parse-p #'(lambda x e))
                (compound (list (metavar 'x #f) (metavar 'e #f))
                          (list 'lambda (nt 'x) (nt 'e))))
  (check-equal? (t1-parse-t #'(pick e e))
                (metafunction 'pick (list (metavar 'e #f) (metavar 'e #f))
                              (list 'pick (nt 'e) (nt 'e))))
  (check-match (t1-parse-t #'(lambda z z))
               (compound (list (prim 'z _) (prim 'z _))
                         (list 'lambda (nt 'x) (nt 'e)))))

;; lang-typechecker : (sort -> type) (symbol -> type) (type type -> boolean)
;;                      -> (values (ast (listof binding) -> tc-template-result)
;;                                 (ast -> tc-pattern-result))
(define (lang-typechecker sort->type metafunction->type subtype?)
  (define (tc-temp ast bindings)
    ;; tc-temps : (listof ast) (listof type) -> void
    ;; given a list of asts and a list of expected types, both of the
    ;; same length, tc-temps checks pairwise for each ast, type pair
    ;; if the ast is a subtype of the expected type
    (define (tc-temps asts expected-tys)
      (for ([ast asts]
            [expected-ty expected-tys])
        (match (tc-temp ast bindings)
          [(tc-template-result actual-ty)
           (unless (subtype? actual-ty expected-ty)
             (raise-arguments-error
              'tc-temp
              "expected a template with a different type"
              "expected" expected-ty
              "got" actual-ty))]
          [#f
           (raise-arguments-error
            'tc-temp
            "template did not typecheck"
            "template" ast)])))

    (match ast
      [(? symbol? s)
       (tc-template-result (sort->type s))]
      [(metavar nt suffix)
       (match (lookup-ty ast bindings)
         [#f (raise-arguments-error
              'tc-temp
              "could not typecheck an unbound variable"
              "variable" ast)]
         [ty (tc-template-result ty)])]
      [(metafunction name args sort)
       (define expected-tys (map nt-symbol (filter nt? (cdr sort))))
       (tc-temps args expected-tys)
       (tc-template-result (metafunction->type name))]
      [(prim p id)
       (error 'tc-temp)]
      [(compound asts sort)
       (define expected-tys (map nt-symbol (filter nt? sort)))
       (tc-temps asts expected-tys)
       (tc-template-result (sort->type sort))]))
  (define (tc-pat ast)
    (define (tc-pats asts expected-tys)
      (append*
       (for/list ([ast asts]
                  [expected-ty expected-tys])
         (match-define (tc-pattern-result actual-ty bindings) (tc-pat ast))
         (if (subtype? actual-ty expected-ty)
             bindings
             (raise-arguments-error
              'tc-pat
              "expected a pattern with a different type"
              "expected" expected-ty
              "got" actual-ty)))))
    (match ast
      [(? symbol? s)
       (tc-pattern-result (sort->type s) '())]
      [(metavar nt suffix)
       (tc-pattern-result nt (list (binding ast nt)))]
      [(metafunction name args sort)
       (raise-arguments-error
        'tc-pat
        "metafunctions cannot be used as a pattern"
        "given metafunction" name)]
      [(prim p id)
       (error 'tc-pat)]
      [(compound asts sort)
       (define expected-tys (map nt-symbol (filter nt? sort)))
       (define bindings (tc-pats asts expected-tys))
       (tc-pattern-result (sort->type sort) bindings)]))
  (values tc-temp tc-pat))

(module+ test
  (define (t1-sort->type sort)
    (match sort
      ['mt 'k]
      [(list 'lambda (nt 'x) (nt 'e)) 'e]
      [(list (nt 'e) (nt 'e)) 'e]))
  (define (t1-metafunction->type name)
    (match name
      ['lookup 'e]
      ['extend 'env]))
  (define (t1-subtype? ty1 ty2)
    (match* (ty1 ty2)
      [('x 'x) #t]
      [('e 'e) #t]
      [('env 'env) #t]
      [('x 'e) #t]
      [(_ _) #f]))
  (define-values (t1-tc-t t1-tc-p)
    (lang-typechecker t1-sort->type t1-metafunction->type t1-subtype?))

  (check-match (t1-tc-t (metavar 'x #f) (list (binding (metavar 'x #f) 'x)))
               (tc-template-result 'x))

  (check-match (t1-tc-t (compound (list (metavar 'x 1) (metavar 'x 1))
                                  (list 'lambda (nt 'x) (nt 'e)))
                        (list (binding (metavar 'x 1) 'x)))
               (tc-template-result 'e))

  (check-match (t1-tc-t (metafunction 'lookup (list (metavar 'x #f) (metavar 'env #f))
                                      (list 'lookup (nt 'x) (nt 'env)))
                        (list (binding (metavar 'x #f) 'x)
                              (binding (metavar 'env #f) 'env)))
               (tc-template-result 'e))

  (check-match (t1-tc-t (compound (list (metavar 'x #f)
                                        (compound (list (metavar 'x #f) (metavar 'x #f))
                                                  (list (nt 'e) (nt 'e))))
                                  (list 'lambda (nt 'x) (nt 'e)))
                        (list (binding (metavar 'x #f) 'x)))
               (tc-template-result 'e))

  (define (unbound-variable) (t1-tc-t (metavar 'x #f) '()))
  (check-exn exn:fail:contract? unbound-variable)


  (check-match (t1-tc-p (compound (list (metavar 'x 1) (metavar 'x 2))
                                  (list 'lambda (nt 'x) (nt 'e))))
               (tc-pattern-result 'e _))

  (check-match (t1-tc-p (compound (list (metavar 'x 1)
                                        (compound (list (metavar 'x 2) (metavar 'e #f))
                                                  (list 'lambda (nt 'x) (nt 'e))))
                                  (list (nt 'e) (nt 'e))))
               (tc-pattern-result 'e _)))

;; compile-cek : id (listof production) id id id (listof step) -> stx
(define (compile-cek lang-id productions c-id e-id k-id steps)
  #'(void))
