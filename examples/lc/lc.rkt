#lang racket
(require "../../cek-metalang/cek-metalang.rkt")
(define-cek lc
  #:grammar
  (e ::= var l (app e e) (quote c) (if e e e) (let binding e) ignore)
  (binding ::= (bp))
  (bp ::= (var e))
  (var ::= variable)
  (l ::= (lam var e))
  (v ::= (clo l env) c)
  (c ::= true false integer)
  (env ::= dummy)
  (k ::= mt (arg e env k) (fn v k) (sel e e env k) (bind var e env k) (ret v k))
  #:control-string e
  #:environment env
  #:continuation k
  #:initial [e --> ((let ([iszero (lam n (zeropimpl n))])
                      (let ([succ (lam n (succimpl n))])
                        (let ([pred (lam n (predimpl n))])
                          (let ([+ (lam m (lam n (addimpl m n)))])
                            (let ([- (lam m (lam n (subimpl m n)))])
                              (let ([* (lam m (lam n (multimpl m n)))])
                                (let ([box (lam b (boximpl b))])
                                  (let ([unbox (lam b (unboximpl b))])
                                    (let ([set-box! (lam b (lam val (setboximpl b val)))])
                                      e)))))))))
                    (emptyenv)
                    mt)]
  #:final [(ignore env_0 (ret v mt)) --> (pprint v)]
  #:step
  [(var env_0 k) --> (ignore env_0 (ret (lookup env_0 var) k))]
  [((lam var e_0) env_0 k) --> (ignore env_0 (ret (clo (lam var e_0) env_0) k))]
  [((quote c) env_0 k) --> (ignore env_0 (ret c k))]
  [((app e_1 e_2) env k) --> (e_1 env (arg e_2 env k))]
  [((if e_test e_then e_else) env k) --> (e_test env (sel e_then e_else env k))]
  [((let ([var e_0]) e_1) env k) --> (e_0 env (bind var e_1 env k))]

  [(ignore env_0 (ret v (arg e env k))) --> (e env (fn v k))]
  [(ignore env_0 (ret v (fn (clo (lam var e) env) k))) --> (e (extend env var v) k)]
  [(ignore env_0 (ret false (sel e_then e_else env k))) --> (e_else env k)]
  [(ignore env_0 (ret v (sel e_then e_else env k))) --> (e_then env k)
   #:unless false v]
  [(ignore env_0 (ret v (bind var e env k))) --> (e (extend env var v) k)])

(module+ main
  (require syntax/parse)
  (define (desugar stx)
    (syntax-parse stx
      #:datum-literals (let let* lambda if app quote true false)
      [(~or :exact-integer true false) #`(quote #,this-syntax)]
      [(app e1 e2)
       #`(app #,(desugar #'e1) #,(desugar #'e2))]
      [(lambda (x) e)
       #`(lam x #,(desugar #'e))]
      [(let ([x0 e0]) e)
       #`(let ([x0 #,(desugar #'e0)]) #,(desugar #'e))]
      [(let* () body)
       (desugar #'body)]
      [(let* ([x0 e0] [x e] ...) body)
       #`(let ([x0 #,(desugar #'e0)])
           #,(desugar
              #`(let* ([x e] ...)
                  body)))]
      [(if e1 e2 e3)
       #`(if #,(desugar #'e1)
             #,(desugar #'e2)
             #,(desugar #'e3))]
      [(e1 e2)
       #`(app
          #,(desugar #'e1)
          #,(desugar #'e2))]
      [_ this-syntax]))

  (define (desugar/lc-term->py stx)
    #;(pretty-print (syntax->datum stx) (current-error-port))
    (lc-term->py (desugar stx)))
  (command-line
   #:program "lc"
   #:once-any
   ["--print-interp" "Print the Python definition of the interpreter"
                     (print-lc-interp)]
   ["--compile-term" ("Read a lc term from stdin and print a function"
                      "main that runs the term's Python definition")
                     (pretty-display (desugar/lc-term->py (read-syntax)))]))
