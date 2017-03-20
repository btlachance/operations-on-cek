#lang racket
(module+ test-ex.adder
  (require
   rackunit
   "cek-metalang.rkt")
  (define-cek lc
    #:expression
    (e ::= var v (e e))
    (var ::= x y z) ;; For now, our language only has three variables
                    ;; Eventually, we can replace this with something
                    ;; like variable-not-otherwise-mentioned
    (v ::= (lam var e))
    #:env
    (env ::= default-env)
    #:continuation
    (k ::= default-mt (arg e env) (fn e env))
    #:step
    [(var env k) --> ((lookup env var) env k)]
    [((e_1 e_2) env k) --> (e_1 env (:: (arg e_2 env) k))]
    [(v env_0 (:: (arg e env) k)) --> (e env (:: (fn v env_0) k))]
    [(v env_0 (:: (fn (lam var e) env) k)) --> (e (extend env var v) k)]))
