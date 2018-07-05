#lang racket
(require (file "~/projects/operations-on-cek/cek-metalang/cek-metalang.rkt"))

(define-cek tl

  #:grammar
  (e ::=
     var
     (quote integer)
     (app op e e)
     (let var e e)
     (ifz e e e)
     ignore)
  (op ::= plus minus)
  (var ::= variable)

  (v ::= integer)

  (env ::= dummy)

  (k ::=
     mt
     (rand2 e env op k)
     (doop op v k)
     (bind var e env k)
     (sel e e env k)
     (ret v k))


  #:control-string e
  #:environment env
  #:continuation k


  #:initial [e --> (e (emptyenv) mt)]
  #:final [(ignore env (ret integer mt)) --> (quote integer)]

  #:step
  [(var env k) --> (ignore env (ret (lookup env var) k))]

  [((quote integer) env k) --> (ignore env (ret integer k))]

  
  [((app op e_1 e_2) env k) --> (e_1 env (rand2 e_2 env op k))]

  [((let var e_var e_body) env k) --> (e_var env (bind var e_body env k))]

  [((ifz e_test e_then e_else) env k) --> (e_test env (sel e_then e_else env k))]

  
  [(ignore env_0 (ret v_1 (rand2 e_2 env op k))) --> (e_2 env (doop op v_1 k))]

  [(ignore env_0 (ret v_2 (doop plus v_1 k))) --> (ignore env_0 (ret v_result k))
   #:where v_result (primadd v_1 v_2)]

  [(ignore env_0 (ret v_2 (doop minus v_1 k))) --> (ignore env_0 (ret v_result k))
   #:where v_result (primsub v_1 v_2)]

  [(ignore env_0 (ret v (bind var e_body env k)))  --> (e_body (extend1 env var v) k)]

  [(ignore env_0 (ret 0 (sel e_then e_else env k))) --> (e_then env k)]

  [(ignore env_0 (ret v (sel e_then e_else env k))) --> (e_else env k)
   #:unless 0 v])
