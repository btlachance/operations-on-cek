#lang racket
(module+ test-ex.adder
  (require
   rackunit
   "cek-metalang.rkt")
  (define-cek lc
    #:expression
    (e ::= x (e e) (lam x e))
    (x ::= default-var)
    (v ::= (lam x e))
    #:env
    (env ::= default-env)
    #:continuation
    (k ::= default-mt (arg e env) (fn e env))
    #:step
    [(x env k) --> ((lookup env x) env k)]
    [((e_1 e_2) env k) --> (e_1 env (:: (arg e_2 env) k))]
    [(v env_0 (:: (arg e env) k)) --> (e env (:: (fn v env_0) k))]
    [(v env_0 (:: (fn (lam x e) env) k)) --> (e (extend env x v) k)]))
