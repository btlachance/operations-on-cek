#lang racket
(require "../../cek-metalang/cek-metalang.rkt")
(define-cek ocalc
  #:grammar
  (e ::=
     var
     l
     (quote c)
     emptyobj
     (app e e)
     (ifz e e e)
     (respond e var e)
     (send e var)
     ignore)
  (var ::= variable)
  (l ::= (lam var e))
  (c ::=
     number
     add1
     sub1)

  (v ::=
     (clo l env)
     c
     (obj env))

  (env ::= dummy)
  (k ::=
     mt
     (arg e env k)
     (op v k)
     (sel e e env k)
     (gethandler e var env k)
     (extend v var k)
     (deliver var k)
     (ret v k))

  #:control-string e
  #:environment env
  #:continuation k

  #:initial [e --> (e (emptyenv) mt)]
  #:final
  ;; If the final result of a program can be either an number or a
  ;; procedure and I want to be able to distinguish between those two
  ;; cases, then I need some kind of disjunction for #:final. Right
  ;; now the only disjunction in the metalanguage is through rule
  ;; sequencing, so that suggests it would be nice to allow multiple
  ;; rules for #:final.
  [(ignore env (ret v mt)) --> (quote 0)]

  #:step
  [(var env k) --> (ignore env (ret (lookup env var) k))])
