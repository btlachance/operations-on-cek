#lang racket
(module+ test-ex.environment
  (require
   rackunit
   "cek-metalang.rkt")
  (define-cek environment
    #:expression
    (e ::= var)
    (var ::= x y z)
    #:val natural
    #:env
    (env ::= default-env)
    #:continuation
    (k ::= default-mt)
    #:step
    [(var env k) --> ((lookup env var) env k)
     #:implemented-by var]))

(module+ test-ex.lc
  (require
   rackunit
   "cek-metalang.rkt")
  (define-cek lc
    #:expression
    (e ::= var v (e e))
    (var ::= x y z) ;; For now, our language only has three variables
                    ;; We can remove this limitation later with
                    ;; something like variable-not-otherwise-mentioned
    (v ::= (lam var e))
    #:val v
    #:env
    (env ::= default-env)
    #:continuation
    (k ::= default-mt (arg e env) (fn v env))
    #:step
    [(var env k) --> ((lookup env var) env k)
     #:implemented-by var]
    [((e_1 e_2) env k) --> (e_1 env (:: (arg e_2 env) k))
     #:implemented-by (e e)]
    [(v env_0 (:: (arg e env) k)) --> (e env (:: (fn v env_0) k))
     #:implemented-by v]
    [(v env_0 (:: (fn (lam var e) env) k)) --> (e (extend env var v) k)
     #:implemented-by v]))
