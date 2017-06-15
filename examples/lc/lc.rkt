#lang racket
(require "../../cek-metalang/cek-metalang.rkt")
(define-cek lc
  #:grammar
  (e ::= var v (e e))
  (var ::= variable)
  (v ::= (lam var e))
  (w ::= (clo v env))
  (env ::= dummy)
  (k ::= mt (arg e env k) (fn v env k))
  #:control-string e
  #:environment env
  #:continuation k
  #:final [(v env_0 mt) --> (pprint v)]
  #:step
  [(var env_0 k) --> (v env k)
   #:where (clo v env) (lookup env_0 var)]
  [((e_1 e_2) env k) --> (e_1 env (arg e_2 env k))]
  [(v env_0 (arg e env k)) --> (e env (fn v env_0 k))]
  [(v env_0 (fn (lam var e) env k)) --> (e (extend env var (clo v env_0)) k)])

(module+ main
  (require syntax/parse)
  (define (desugar stx)
    (syntax-parse stx
      #:datum-literals (let lambda)
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
