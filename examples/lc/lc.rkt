#lang racket
(require "../../cek-metalang/cek-metalang.rkt")
(define-cek lc
  #:grammar
  (e ::= var v (e e) (if e e e) (iszero e) (succ e) (pred e))
  (var ::= variable)
  (v ::= (lam var e) bool nat)
  (bool ::= true false)
  (nat ::= z (s nat))
  (w ::= (clo v env))
  (env ::= dummy)
  (k ::= mt (arg e env k) (fn v env k) (sel e e env k) (zerop env k)
     (add1 env k) (sub1 env k))
  #:control-string e
  #:environment env
  #:continuation k
  #:final [(v env_0 mt) --> (pprint v)]
  #:step
  [(var env_0 k) --> (v env k)
   #:where (clo v env) (lookup env_0 var)]
  [((e_1 e_2) env k) --> (e_1 env (arg e_2 env k))]
  [((if e_test e_then e_else) env k) --> (e_test env (sel e_then e_else env k))]
  [((iszero e_0) env k) --> (e_0 env (zerop env k))]
  [((succ e_0) env k) --> (e_0 env (add1 env k))]
  [((pred e_0) env k) --> (e_0 env (sub1 env k))]


  [(v env_0 (arg e env k)) --> (e env (fn v env_0 k))]
  [(v env_0 (fn (lam var e) env k)) --> (e (extend env var (clo v env_0)) k)]
  [(false env_0 (sel e_then e_else env k)) --> (e_else env k)]
  [(v env_0 (sel e_then e_else env k)) --> (e_then env k)
   #:unless false v]
  [(nat env_0 (zerop env k)) --> (false env k)
   #:unless z nat]
  [(z env_0 (zerop env k)) --> (true env k)]
  [(nat env_0 (add1 env k)) --> ((s nat) env k)]
  [((s nat) env_0 (sub1 env k)) --> (nat env k)])

(module+ main
  (require syntax/parse)
  (define (desugar stx)
    (syntax-parse stx
      #:datum-literals (let* let lambda if)
      [(lambda (x) e)
       #`(lam x #,(desugar #'e))]
      [(let (x e) body)
       (define e* (desugar #'e))
       (define body* (desugar #'body))
       #`((lam x #,body*) #,e*)]
      [(let* () body)
       (desugar #'body)]
      [(let* ([x0 e0] [x e] ...) body)
       (define e0* (desugar #'e0))
       #`((lam x0 #,(desugar #'(let* ([x e] ...) body))) #,e0*)]
      [(e1 e2)
       #`(#,(desugar #'e1)
          #,(desugar #'e2))]
      [(if e1 e2 e3)
       #`(if #,(desugar #'e1)
             #,(desugar #'e2)
             #,(desugar #'e3))]
      [_ this-syntax]))

  (define desugar/lc-term->py (compose lc-term->py desugar))
  (command-line
   #:program "lc"
   #:once-any
   ["--print-interp" "Print the Python definition of the interpreter"
                     (print-lc-interp)]
   ["--compile-term" ("Read a lc term from stdin and print a function"
                      "main that runs the term's Python definition")
                     (pretty-display (desugar/lc-term->py (read-syntax)))]))
