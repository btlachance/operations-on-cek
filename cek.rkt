#lang racket
(require rackunit)

(module+ test
  (check-equal? (extend empty-env 'x '(lam x x)) '((x (lam x x)))))
(define empty-env '())
(define (extend env x v)
  (cons `(,x ,v) env))
(module+ test
  (define env1 (extend (extend empty-env 'y '(lam y y)) 'x '(lam x x)))
  (check-equal? (lookup env1 'x) '(lam x x))
  (check-equal? (lookup env1 'y) '(lam y y)))
(define (lookup env x)
  (match (assoc x env)
    [`(,_ ,v) v]
    [_ (error 'not-found "could not find ~a in environment" x)]))

(module+ test
  (check-true (value? '(lam x x)))
  (check-true (value? '(lam x (lam y x)))))
(define (value? e)
  (match e
    [`(lam ,x ,e*) #t]
    [_ #f]))

;; e ::= x | (lam x e) | e e
(define (step . config)
  (match config
    [`((,e1 ,e2) ,env ,k)
     (values e1 env `(arg ,e2 ,env . ,k))]
    [`(,(? value? v) ,env (arg ,e-arg ,env* . ,k))
     (values e-arg env* `(fn ,v ,env . ,k))]
    [`(,(? value? v) ,env (fn (lam ,x ,e) ,env* . ,k))
     (values e (extend env* x v) k)]
    [`(,x ,env ,k)
     (values (lookup env x) env k)]))

(module+ test
  (check-equal? (run '((lam x x) (lam y y)))
                '(lam y y))
  (check-equal? (run '(((lam x (lam y x)) (lam z z)) (lam w w)))
                '(lam z z))
  (check-equal? (run '(((lam x (lam x x)) (lam z z)) (lam w w)))
                '(lam w w)))
(define (run e)
  (let loop ([c e] [env empty-env] [k '()])
    (define-values (c* env* k*) (step c env k))
    (match* (c* k*)
      [((? value? v) '()) v]
      [(_ _) (loop c* env* k*)])))
