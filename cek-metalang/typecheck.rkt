#lang racket
(require "rep.rkt")
(provide lang-typechecker)

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
  (require rackunit)
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
