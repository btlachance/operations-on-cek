#lang racket
(module test-ex.lc racket
  (require "../cek-metalang/cek-metalang.rkt")
  (define-cek lc
    #:grammar
    (e ::= var v (e e))
    (var ::= x y z) ;; For now, our language only has three variables
                    ;; We can remove this limitation later with some
                    ;; kind of variable or symbol primitive
    (v ::= (lam var e))
    (w ::= (clo v env))
    (env ::= dummy)
    (k ::= mt (arg e env k) (fn v env k))
    #:control-string e
    #:environment env
    #:continuation k
    #:step
    [(var env_0 k) --> (v env k)
     #:where (clo v env) (lookup env_0 var)]
    [((e_1 e_2) env k) --> (e_1 env (arg e_2 env k))]
    [(v env_0 (arg e env k)) --> (e env (fn v env_0 k))]
    [(v env_0 (fn (lam var e) env k)) --> (e (extend env var (clo v env_0)) k)]))
