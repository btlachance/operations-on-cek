#lang racket
(require "../../cek-metalang/cek-metalang.rkt")
(define-cek lc
  #:grammar
  (term ::= modbody modform)
  (modbody ::= (modbegin modforms))
  (modforms ::= mfnil (mf modform modforms))
  (modform ::= gtopform)
  (gtopform ::= e (define var e))

  (e ::= var l (app e e) (quote c) (if e e e) (let binding e) ignore)
  (binding ::= (bp))
  (bp ::= (p var e))
  (var ::= variable)
  (l ::= (lam var e))
  (v ::= (clo l env) c)
  (c ::= true false integer)

  (env ::= dummy)

  (k ::= modk expk mt)
  (modk ::= (binddefs modforms env modforms k))
  (expk ::= (arg e env k) (fn v k) (sel e e env k) (bind var e env k) (ret v k)
        (evaldefs modform modforms env k))
  #:control-string term
  #:environment env
  #:continuation k
  #:initial [(modbegin modforms) -->
             ((modbegin
               (mf (define zero? (lam n (zeropimpl n)))
                   (mf (define add1 (lam n (succimpl n)))
                       (mf (define sub1 (lam n (predimpl n)))
                           (mf (define + (lam m (lam n (addimpl m n))))
                               (mf (define - (lam m (lam n (subimpl m n))))
                                   (mf (define * (lam m (lam n (multimpl m n))))
                                       (mf (define box (lam b (boximpl b)))
                                           (mf (define unbox  (lam b (unboximpl b)))
                                               (mf (define set-box! (lam b (lam val (setboximpl b val))))
                                                   modforms))))))))))
              (emptyenv)
              mt)]
  #:final [(ignore env_0 (ret v mt)) --> (pprint v)]
  #:step
  ;; IDK what to do when the module body is empty... I don't yet have
  ;; a void value, but I guess that's what I need.
  [((modbegin (mf modform_0 modforms_0)) env k) -->
   (modform_1 env (binddefs mfnil env modforms_1 k))
   #:where (mf modform_1 modforms_1) (modformsreverse (mf modform_0 modforms_0))]

  [((define var e_0) env (binddefs modforms_bound env_0 (mf modform modforms_unbound) k))
   -->
   (modform env (binddefs (mf (define var e_0) modforms_bound) env_1 modforms_unbound k))
   #:where v (mkcell false)
   #:where env_1 (extend env_0 var v)]
  [(e_0 env (binddefs modforms_bound env_0 (mf modform modforms_unbound) k))
   -->
   (modform env (binddefs (mf e_0 modforms_bound) env_0 modforms_unbound k))]

  [((define var e_0) env (binddefs modforms_bound env_0 mfnil k))
   -->
   (e_0 env_1 (evaldefs (define var e_0) modforms_bound env_1 k))
   #:where v (mkcell false)
   #:where env_1 (extend env_0 var v)]
  ;; Tricky here: we assume that modform is identical to (define var
  ;; e_0). That should be the case, given how a modform is duplicated
  ;; when it's popped out of evaldefs
  [((define var e_0) env (evaldefs modform modforms env_0 k))
   -->
   (e_0 env_0 (evaldefs modform modforms env_0 k))]
  [(ignore env (ret v (evaldefs (define var e_0) (mf modform modforms) env_0 k)))
   -->
   (modform env_0 (evaldefs modform modforms env_0 k))
   #:where v_ignore (setcell var env_0 v)]
  [(ignore env (ret v (evaldefs (define var e_0) mfnil env_0 k)))
   -->
   (ignore env (ret v k))]

  [(e_0 env (binddefs modforms_bound env_0 mfnil k))
   -->
   (e_0 env_0 (evaldefs e_0 modforms_bound env_0 k))]
  [(ignore env (ret v (evaldefs e_0 (mf modform modforms) env_0 k)))
   -->
   (modform env_0 (evaldefs modform modforms env_0 k))]
  [(ignore env (ret v (evaldefs e_0 mfnil env_0 k)))
   -->
   (ignore env (ret v k))]

  [(var env_0 expk) --> (ignore env_0 (ret (lookup env_0 var) expk))]
  [((lam var e_0) env_0 expk) --> (ignore env_0 (ret (clo (lam var e_0) env_0) expk))]
  [((quote c) env_0 expk) --> (ignore env_0 (ret c expk))]
  [((app e_1 e_2) env expk) --> (e_1 env (arg e_2 env expk))]
  [((if e_test e_then e_else) env expk) --> (e_test env (sel e_then e_else env expk))]
  #;[((let ([p var e_0]) e_1) env expk) --> (e_0 env (bind var e_1 env expk))]

  [(ignore env_0 (ret v (arg e env expk))) --> (e env (fn v expk))]
  [(ignore env_0 (ret v (fn (clo (lam var e) env) expk))) --> (e (extend env var v) expk)]
  [(ignore env_0 (ret false (sel e_then e_else env expk))) --> (e_else env expk)]
  [(ignore env_0 (ret v (sel e_then e_else env expk))) --> (e_then env expk)
   #:unless false v]
  #;[(ignore env_0 (ret v (bind var e env expk))) --> (e (extend env var v) expk)])

(module+ main
  (require syntax/parse)
  (define (ignored-modform? stx)
    (syntax-parse stx
      #:datum-literals ([mb #%module-begin]
                        [app #%app]
                        [c-w-v call-with-values]
                        define-syntaxes
                        module)
      [(module id lang
         _ ...)
       (equal? 'configure-runtime (syntax-e #'id))]
      [(define-syntaxes _ ...) #t]
      [_ #f]))

  (define (kernel->core stx)
    (syntax-parse stx
      #:datum-literals (module
                        [mb #%module-begin]
                        [app #%app]
                        [lam lambda]
                        [c-w-v call-with-values]
                        [p-v print-values]
                        define-values
                        if)
      [(module id lang
         (mb form ...+))
       #`(modbegin
          #,(foldr
             (lambda (form rest)
               #`(mf #,(kernel->core form) #,rest))
             #'mfnil
             (filter-not ignored-modform? (syntax->list #'(form ...)))))]
      [(app c-w-v (lam () exp) p-v)
       (kernel->core #'exp)]
      [(app e1 e2 e-rest ...)
       (foldr
        (lambda (arg app-so-far)
          #`(app #,app-so-far #,(kernel->core arg)))
        #`(app #,(kernel->core #'e1) #,(kernel->core #'e2))
        (syntax->list #'(e-rest ...)))]
      [(lam (x x-rest ...) e)
       (foldr
        (lambda (id lam-so-far)
          #`(lam id #,lam-so-far))
        #`(lam x #,(kernel->core #'e))
        (syntax->list #'(x-rest ...)))]
      [(define-values (id) e)
       #`(define id #,(kernel->core #'e))]
      [(if e1 e2 e3)
       #`(if #,(kernel->core #'e1)
             #,(kernel->core #'e2)
             #,(kernel->core #'e3))]
      [_ this-syntax]))

  (define (desugar stx)
    (syntax-parse stx
      #:datum-literals (let let* lambda if app quote true false)
      [(~or :exact-integer true false) #`(quote #,this-syntax)]
      [(app e1 e2)
       #`(app #,(desugar #'e1) #,(desugar #'e2))]
      [(lambda (x) e)
       #`(lam x #,(desugar #'e))]
      [(let ([x0 e0]) e)
       #`(let ([p x0 #,(desugar #'e0)]) #,(desugar #'e))]
      [(let* () body)
       (desugar #'body)]
      [(let* ([x0 e0] [x e] ...) body)
       #`(let ([p x0 #,(desugar #'e0)])
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
  (define (corify/lc-term->py stx)
    #;(pretty-print (syntax->datum stx) (current-error-port))
    #;(pretty-print (syntax->datum (kernel->core stx)) (current-error-port))
    (lc-term->py (kernel->core stx)))

  (command-line
   #:program "lc"
   #:once-any
   ["--print-interp" "Print the Python definition of the interpreter"
                     (print-lc-interp)]
   ["--compile-term" ("Read a lc term from stdin and print a function"
                      "main that runs the term's Python definition")
                     (pretty-display (corify/lc-term->py (read-syntax)))]))
