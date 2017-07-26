#lang racket
(require "../../cek-metalang/cek-metalang.rkt")
(define-cek lc
  #:grammar
  (term ::= modbody modform)
  (modbody ::= (modbegin modforms))
  (modforms ::= mfnil (mf modform modforms))
  (modform ::= gtopform)
  (gtopform ::= e (define var e))

  (e ::= var l (app e es) (quote c) (if e e e) (let binding e) ignore)
  (es ::= esnil (el e es))
  (binding ::= (bp))
  (bp ::= (p var e))
  (var ::= variable)
  (vars ::= varsnil (varl var vars))
  (l ::= (lam vars e))
  (v ::= (clo l env) c)
  (vs ::= vsnil (vl v vs))
  (c ::= true false integer)

  (env ::= dummy)

  (k ::= modk expk mt)
  (modk ::= (binddefs modforms modforms k))
  (expk ::= (args es env k) (fn v vs es env k) (sel e e env k) (bind var e env k) (ret v k)
        (evaldefs modform modforms env k))
  #:control-string term
  #:environment env
  #:continuation k
  #:initial [(modbegin modforms) -->
             ((modbegin
               (mf (define zero? (lam (varl n varsnil) (zeropimpl n)))
                   (mf (define add1 (lam (varl n varsnil) (succimpl n)))
                       (mf (define sub1 (lam (varl n varsnil) (predimpl n)))
                           (mf (define + (lam (varl m (varl n varsnil)) (addimpl m n)))
                               (mf (define - (lam (varl m (varl n varsnil)) (subimpl m n)))
                                   (mf (define * (lam (varl m (varl n varsnil)) (multimpl m n)))
                                       (mf (define box (lam (varl b varsnil) (boximpl b)))
                                           (mf (define unbox  (lam (varl b varsnil) (unboximpl b)))
                                               (mf (define set-box! (lam (varl b (varl val varsnil)) (setboximpl b val)))
                                                   modforms))))))))))
              (emptyenv)
              mt)]
  #:final [(ignore env_0 (ret v mt)) --> (pprint v)]
  #:step
  ;; IDK what to do when the module body is empty... I don't yet have
  ;; a void value, but I guess that's what I need.
  [((modbegin (mf modform_0 modforms_0)) env k) -->
   (modform_1 env (binddefs mfnil modforms_1 k))
   #:where (mf modform_1 modforms_1) (modformsreverse (mf modform_0 modforms_0))]

  [(gtopform env_0 (binddefs modforms_bound (mf modform modforms_unbound) k))
   -->
   (modform env_1 (binddefs (mf gtopform modforms_bound) modforms_unbound k))
   #:where (define var e_0) gtopform
   #:where v (mkcell false)
   #:where env_1 (extend1 env_0 var v)]
  [(e_0 env (binddefs modforms_bound (mf modform modforms_unbound) k))
   -->
   (modform env (binddefs (mf e_0 modforms_bound) modforms_unbound k))]
  [(gtopform env_0 (binddefs modforms_bound mfnil k))
   -->
   (gtopform env_1 (evaldefs gtopform modforms_bound env_1 k))
   #:where (define var e_0) gtopform
   #:where v (mkcell false)
   #:where env_1 (extend1 env_0 var v)]
  [(e_0 env (binddefs modforms_bound mfnil k))
   -->
   (e_0 env (evaldefs e_0 modforms_bound env k))]

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

  [(ignore env (ret v (evaldefs e_0 (mf modform modforms) env_0 k)))
   -->
   (modform env_0 (evaldefs modform modforms env_0 k))]
  [(ignore env (ret v (evaldefs e_0 mfnil env_0 k)))
   -->
   (ignore env (ret v k))]

  [(var env_0 expk) --> (ignore env_0 (ret (lookup env_0 var) expk))]
  [(l env_0 expk) --> (ignore env_0 (ret (clo l env_0) expk))]
  [((quote c) env_0 expk) --> (ignore env_0 (ret c expk))]
  [((app e_1 es) env expk) --> (e_1 env (args es env expk))]
  [((if e_test e_then e_else) env expk) --> (e_test env (sel e_then e_else env expk))]
  [((let ([p var e_0]) e_1) env expk) --> (e_0 env (bind var e_1 env expk))]

  [(ignore env_0 (ret v (args esnil env_1 expk))) --> (e env expk)
   #:where (clo (lam varsnil e) env) v]
  [(ignore env_0 (ret v (args (el e es) env expk))) --> (e env (fn v vsnil es env expk))]
  [(ignore env_0 (ret v (fn v_op vs (el e es) env expk))) --> (e env (fn v_op (vl v vs) es env expk))]
  [(ignore env_0 (ret v (fn (clo (lam vars e) env) vs_rev esnil env_1 expk))) --> (e (extend env vars vs) expk)
   #:where vs (vsreverse (vl v vs_rev))]
  [(ignore env_0 (ret false (sel e_then e_else env expk))) --> (e_else env expk)]
  [(ignore env_0 (ret v (sel e_then e_else env expk))) --> (e_then env expk)
   #:unless false v]
  [(ignore env_0 (ret v (bind var e env expk))) --> (e (extend1 env var v) expk)])

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
                        quote
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
      [(app e1 e-rest ...)
       #`(app
          #,(kernel->core #'e1)
          #,(foldr
             (lambda (arg args) #`(el #,(kernel->core arg) #,args))
             #'esnil
             (syntax->list #'(e-rest ...))))]
      [(lam (xs ...) e)
       #`(lam
          #,(foldr
             (lambda (id ids) #`(varl #,id #,ids))
             #'varsnil
             (syntax->list #'(xs ...)))
          #,(kernel->core #'e))]
      [(define-values (id) e)
       #`(define id #,(kernel->core #'e))]
      [(if e1 e2 e3)
       #`(if #,(kernel->core #'e1)
             #,(kernel->core #'e2)
             #,(kernel->core #'e3))]
      [(quote #t) #'(quote true)]
      [(quote #f) #'(quote false)]
      [_ this-syntax]))

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
