#lang racket
(require "rep.rkt")
(provide lang-typechecker)

;; a type is a symbol representing the name of a nonterminal from some
;; production

;; lang-typechecker : (hash sort type) (hash symbol type) (type type -> boolean)
;;                      -> (values (ast (listof binding) -> tc-template-result)
;;                                 (ast -> tc-pattern-result))
(define (lang-typechecker sort->type metafunction->type subtype?)
  (define (tc-temp ast bindings)
    ;; tc-temps : (listof ast) (listof type) -> void
    ;; given a list of asts and a list of expected types, both of the
    ;; same length, tc-temps checks pairwise for each ast, type pair
    ;; if the ast is a subtype of the expected type
    (match ast
      [(? symbol? s)
       (tc-template-result (hash-ref sort->type s))]
      [(metavar nt suffix)
       (match (lookup-ty ast bindings)
         [#f (raise-arguments-error
              'tc-temp
              "could not typecheck an unbound variable"
              "variable" ast)]
         [ty (tc-template-result ty)])]
      [(metafunction name args sort)
       (define expected-tys (map nt-symbol (filter nt? (cdr sort))))
       (tc-temps/expecteds args expected-tys bindings)
       (tc-template-result (hash-ref metafunction->type name))]
      [(prim p data)
       ((prim-data-tc-temp data) p)]
      [(compound asts sort)
       (define expected-tys (map nt-symbol (filter nt? sort)))
       (tc-temps/expecteds asts expected-tys bindings)
       (tc-template-result (hash-ref sort->type sort))]))
  (define (tc-temps/expecteds asts expecteds bindings)
    (for ([ast asts]
          [expected expecteds])
      (match (tc-temp ast bindings)
        [(tc-template-result ty)
         (unless (subtype? ty expected)
           (raise-user-error
            'compile-cek
            "expected template ~a to have type ~a but got ~a" ast expected ty))]
        [_ (raise-user-error
            'compile-cek
            "could not typecheck template ~a" ast)])))
  (define (tc-pat ast)
    (match ast
      [(? symbol? s)
       (tc-pattern-result (hash-ref sort->type s) '())]
      [(metavar nt suffix)
       (tc-pattern-result nt (list (binding ast nt)))]
      [(metafunction name args sort)
       (raise-arguments-error
        'tc-pat
        "metafunctions cannot be used as a pattern"
        "given metafunction" name)]
      [(prim p data)
       ((prim-data-tc-pat data) p)]
      [(compound asts sort)
       (define expected-tys (map nt-symbol (filter nt? sort)))
       (define bindings (tc-pats/expecteds asts expected-tys))
       (tc-pattern-result (hash-ref sort->type sort) bindings)]))
  (define (tc-pats/expecteds asts expecteds)
    (for/fold ([bs '()])
              ([ast asts]
               [expected expecteds])
      (match (tc-pat ast)
        [(tc-pattern-result ty bindings)
         (if (subtype? ty expected)
             (append bindings bs)
             (raise-user-error
              'compile-cek
              "expected pattern ~a to have type ~a but got ~a" ast expected ty))]
        [_ (raise-user-error
            'compile-cek
            "could not typecheck pattern ~a" ast)])))
  (define (tc-ast*s ast*s)
    (define (tc-ast* bindings ast*)
      (match ast*
        [(pat* ast expected-ty)
         (tc-pats/expecteds (list ast) (list expected-ty))]
        [(where* temp pat)
         (match (tc-temp temp bindings)
           [(tc-template-result ty)
            (tc-pats/expecteds (list pat) (list ty))]
           [_ (raise-user-error
               'compile-cek
               "could not typecheck template ~a" temp)])]
        [(temp* ast expected-ty)
         (tc-temps/expecteds (list ast) (list expected-ty) bindings)
         bindings]))
    (foldl tc-ast* ast*s '())
    (void))
  (values tc-temp tc-pat tc-temps/expecteds tc-pats/expecteds tc-ast*s))

(module+ test
  (require rackunit)
  (define t1-sort->type
    (hash 'mt 'k
          (list 'lambda (nt 'x) (nt 'e)) 'e
          (list (nt 'e) (nt 'e)) 'e))
  (define t1-metafunction->type
    (hash
     'lookup 'e
     'extend 'env))
  (define (t1-subtype? ty1 ty2)
    (match* (ty1 ty2)
      [('x 'x) #t]
      [('e 'e) #t]
      [('env 'env) #t]
      [('x 'e) #t]
      [(_ _) #f]))
  (define-values (t1-tc-t t1-tc-p t1-tc-ts/expct t1-tc-ps/expct t1-tc-ast*s)
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
