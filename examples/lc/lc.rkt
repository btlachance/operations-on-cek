#lang racket
(require "../../cek-metalang/cek-metalang.rkt")
(define-cek lc
  #:grammar
  (term ::= modbody modform)
  (modbody ::= (modbegin modforms))
  (modforms ::= mfnil (mf modform modforms))
  (modform ::= gtopform)
  (gtopform ::= e (define var e))

  (result ::= v vs)
  (es ::= esnil (el e es))
  (vs ::= vsnil (vl v vs))
  (vars ::= varsnil (varl var vars))
  (valuesbinds ::= valuesbindsnil (vbl valuesbind valuesbinds))

  (e ::= var l (app e es) (quote c) (if e e e) (letvalues valuesbinds e es) (letrecvalues valuesbinds e es)
     (values var) ignore
     (car e) (cdr e) (nullp e) (mkcons e e) (apply e e) (mkvoid) (cwv var var))
  (valuesbind ::= (vb vars e))
  (binding ::= (bp))
  (bp ::= (p var e))
  (var ::= variable)
  (l ::= (lam vars e es) (lamrest vars var e es))
  (v ::= (clo l env) c (cons v v) undefined voidv)
  (c ::= nil true false integer string (sym var))

  (env ::= dummy)

  (k ::= modk expk mt)
  (modk ::= (binddefs modforms modforms k))
  (expk ::= (fn vs es env k) (sel e e env k) (ret result k)
        (cark k) (cdrk k) (nullk k) (consr e env k) (pair v k) (getargs e env k) (applyk v k)
        (evaldefs modform modforms env k) (bindvaluesk env vars valuesbinds env es k)
        (bindrec valuesbinds k) (bindvarscells vars k) (evalrec valuesbinds es k) (setcellsk vars env expk)
        (cwvk var env k) (expsk env es k))
  #:control-string term
  #:environment env
  #:continuation k
  #:initial [(modbegin modforms) -->
             ((modbegin
               (mf (define zero? (lam (varl n varsnil) (zeropimpl n) esnil))
               (mf (define add1 (lam (varl n varsnil) (succimpl n) esnil))
               (mf (define sub1 (lam (varl n varsnil) (predimpl n) esnil))
               (mf (define + (lam (varl m (varl n varsnil)) (addimpl m n) esnil))
               (mf (define - (lam (varl m (varl n varsnil)) (subimpl m n) esnil))
               (mf (define * (lam (varl m (varl n varsnil)) (multimpl m n) esnil))
               (mf (define box (lam (varl b varsnil) (boximpl b) esnil))
               (mf (define unbox  (lam (varl b varsnil) (unboximpl b) esnil))
               (mf (define set-box! (lam (varl b (varl val varsnil)) (setboximpl b val) esnil))
               (mf (define cons (lam (varl val1 (varl val2 varsnil)) (mkcons val1 val2) esnil))
               (mf (define car (lam (varl val varsnil) (car val) esnil))
               (mf (define cdr (lam (varl val varsnil) (cdr val) esnil))
               (mf (define null? (lam (varl val varsnil) (nullp val) esnil))
               (mf (define foldl (lam (varl fn (varl init (varl xs varsnil)))
                                   (if (app null? (el xs esnil))
                                       init
                                       (app foldl (el fn (el (app fn (el (app car (el xs esnil)) (el init esnil))) (el (app cdr (el xs esnil)) esnil))))) esnil))
               (mf (define print (lam (varl val varsnil) (printimpl val) esnil))
               (mf (define < (lam (varl m (varl n varsnil)) (ltimpl m n) esnil))
               (mf (define equal? (lam (varl v1 (varl v2 varsnil)) (eqlimpl v1 v2) esnil))
               (mf (define apply (lam (varl fun (varl args varsnil)) (apply fun args) esnil))
               (mf (define vector (lamrest varsnil args (vectorimpl args) esnil))
               (mf (define vector-ref (lam (varl vec (varl pos varsnil)) (vecrefimpl vec pos) esnil))
               (mf (define vector-length (lam (varl vec varsnil) (veclengthimpl vec) esnil))
               (mf (define current-command-line-arguments (lam varsnil (quote 0) esnil))
               (mf (define void (lamrest varsnil args (mkvoid) esnil))
               (mf (define symbol? (lam (varl s varsnil) (issymbolimpl s) esnil))
               (mf (define values (lamrest varsnil args (values args) esnil))
               (mf (define call-with-values (lam (varl gen (varl recv varsnil)) (cwv gen recv) esnil))
                 modforms)))))))))))))))))))))))))))
              (emptyenv)
              mt)]
  #:final [(ignore env_0 (ret v mt)) --> ignore]
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
   #:where v (mkcell undefined)
   #:where env_1 (extend1 env_0 var v)]
  [(e_0 env (binddefs modforms_bound (mf modform modforms_unbound) k))
   -->
   (modform env (binddefs (mf e_0 modforms_bound) modforms_unbound k))]
  [(gtopform env_0 (binddefs modforms_bound mfnil k))
   -->
   (gtopform env_1 (evaldefs gtopform modforms_bound env_1 k))
   #:where (define var e_0) gtopform
   #:where v (mkcell undefined)
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
  [((quote c) env_0 expk) --> (ignore env_0 (ret c expk))
   #:unless (sym var) c]
  [((quote (sym var)) env_0 expk) --> (ignore env_0 (ret v expk))
   #:where v (mksymbol var)]
  [((app e_1 es) env expk) --> (e_1 env (fn vsnil es env expk))]
  [((if e_test e_then e_else) env expk) --> (e_test env (sel e_then e_else env expk))]
  [((letvalues valuesbinds e_body0 es_body) env expk)
   -->
   (ignore env (bindvaluesk env varsnil valuesbinds env (el e_body0 es_body) expk))]
  [((letrecvalues valuesbinds e_body0 es_body) env expk)
   -->
   (ignore env (bindrec valuesbinds (evalrec valuesbinds (el e_body0 es_body) expk)))]
  [((car e_0) env k) --> (e_0 env (cark k))]
  [((cdr e_0) env k) --> (e_0 env (cdrk k))]
  [((nullp e_0) env k) --> (e_0 env (nullk k))]
  [((mkcons e_1 e_2) env k) --> (e_1 env (consr e_2 env k))]
  [((apply e_1 e_2) env k) --> (e_1 env (getargs e_2 env k))]
  [((mkvoid) env k) --> (ignore env (ret voidv k))]
  [((values var) env expk) --> (ignore env (ret vs expk))
   #:where vs (vlisttovs (lookup env var))]
  [((cwv var_gen var_recv) env expk) --> ((app var_gen esnil) env (cwvk var_recv env expk))]

  [(ignore env_0 (ret v (fn vs es env expk))) --> (ignore env_0 (fn (vl v vs) es env expk))]
  [(ignore env_0 (fn vs (el e es) env expk)) --> (e env (fn vs es env expk))]
  [(ignore env_0 (fn vs esnil env_1 expk)) --> (ignore env_0 (expsk env (el e es) expk))
   #:where (vl v vs_args) (vsreverse vs)
   #:where (clo (lam vars e es) env_clo) v
   #:where env (extend env_clo vars vs_args)]
  [(ignore env_0 (fn vs esnil env_1 expk)) --> (ignore env_0 (expsk env (el e es) expk))
   #:where (vl v vs_args) (vsreverse vs)
   #:where (clo (lamrest vars var_rest e es) env_clo) v
   #:where env (extendrest env_clo vars var_rest vs_args)]
  [(ignore env_0 (ret false (sel e_then e_else env expk))) --> (e_else env expk)]
  [(ignore env_0 (ret v (sel e_then e_else env expk))) --> (e_then env expk)
   #:unless false v]
  [(ignore env_0 (expsk env (el e esnil) expk)) --> (e env expk)]
  [(ignore env_0 (expsk env (el e (el e_next es)) expk)) --> (e env (expsk env (el e_next es) expk))]
  [(ignore env_0 (ret result (expsk env es expk))) --> (ignore env_0 (expsk env es expk))]

  [(ignore env (bindvaluesk env_arg varsnil valuesbindsnil env_acc es expk))
   -->
   (ignore env (expsk env_acc es expk))]
  [(ignore env (bindvaluesk env_arg varsnil (vbl (vb vars e_vars) valuesbinds) env_acc es expk))
   -->
   (e_vars env_arg (bindvaluesk env_arg vars valuesbinds env_acc es expk))]
  [(ignore env (ret v (bindvaluesk env_arg (varl var varsnil) valuesbinds env_acc0 es expk)))
   -->
   (ignore env (bindvaluesk env_arg varsnil valuesbinds env_acc1 es expk))
   #:where env_acc1 (extend1 env_acc0 var v)]
  [(ignore env (ret (vl v vsnil) (bindvaluesk env_arg (varl var varsnil) valuesbinds env_acc0 es expk)))
   -->
   (ignore env (bindvaluesk env_arg varsnil valuesbinds env_acc1 es expk))
   #:where env_acc1 (extend1 env_acc0 var v)]
  [(ignore env (ret vs (bindvaluesk env_arg vars valuesbinds env_acc0 es expk)))
   -->
   (ignore env (bindvaluesk env_arg varsnil valuesbinds env_acc1 es expk))
   #:where env_acc1 (extend env_acc0 vars vs)]

  [(ignore env (bindrec valuesbindsnil expk)) --> (ignore env expk)]
  [(ignore env (bindrec (vbl (vb vars e) valuesbinds) expk))
   -->
   (ignore env (bindvarscells vars (bindrec valuesbinds expk)))]
  [(ignore env (bindvarscells varsnil expk)) --> (ignore env expk)]
  [(ignore env (bindvarscells (varl var vars) expk)) --> (ignore env_1 (bindvarscells vars expk))
   #:where v (mkcell undefined)
   #:where env_1 (extend1 env var v)]
  [(ignore env (evalrec valuesbindsnil es expk)) --> (ignore env (expsk env es expk))]
  [(ignore env (evalrec (vbl (vb vars e) valuesbinds) es expk))
   -->
   (e env (setcellsk vars env (evalrec valuesbinds es expk)))]
  [(ignore env_0 (ret v (setcellsk (varl var varsnil) env expk))) --> (ignore env expk)
   #:where v_ignore (setcell var env v)]
  [(ignore env_0 (ret (vl v vsnil) (setcellsk (varl var varsnil) env expk))) --> (ignore env expk)
   #:where v_ignore (setcell var env v)]
  [(ignore env_0 (ret vs (setcellsk vars env expk))) --> (ignore env expk)
   #:where v_ignore (setcells vars env vs)]

  [(ignore env_0 (ret (cons v_1 v_2) (cark expk))) --> (ignore env_0 (ret v_1 expk))]
  [(ignore env_0 (ret (cons v_1 v_2) (cdrk expk))) --> (ignore env_0 (ret v_2 expk))]
  [(ignore env_0 (ret nil (nullk expk))) --> (ignore env_0 (ret true expk))]
  [(ignore env_0 (ret v (nullk expk))) --> (ignore env_0 (ret false expk))
   #:unless nil v]
  [(ignore env_0 (ret v (consr e env expk))) --> (e env (pair v expk))]
  [(ignore env_0 (ret v_right (pair v_left expk))) --> (ignore env_0 (ret (cons v_left v_right) expk))]
  [(ignore env_0 (ret v (getargs e_args env expk))) --> (e_args env (applyk v expk))]
  [(ignore env_0 (ret v (applyk (clo (lam vars e es) env) expk))) --> (ignore env_0 (expsk (extend env vars vs) (el e es) expk))
   #:where vs (vlisttovs v)]
  [(ignore env_0 (ret v (applyk (clo (lamrest vars var_rest e es) env) expk))) --> (ignore env_0 (expsk (extendrest env vars var_rest vs) (el e es) expk))
   #:where vs (vlisttovs v)]
  [(ignore env_0 (ret v (cwvk var_recv env expk))) --> (ignore env_0 (fn (vl v (vl v_recv vsnil)) esnil env_0 expk))
   #:where v_recv (lookup env var_recv)]
  [(ignore env_0 (ret vs_vals (cwvk var_recv env expk))) --> (ignore env_0 (fn vs esnil env_0 expk))
   #:where v_recv (lookup env var_recv)
   #:where vs (vsreverse (vl v_recv vs_vals))])

(module+ main
  (require syntax/parse)
  (define (ignored-modform? stx)
    (syntax-parse stx
      #:datum-literals (module define-syntaxes define-values
                        #%require)
      [(module id _
         _ ...)
       (equal? (syntax-e #'id) 'configure-runtime)]
      [(define-syntaxes _ ...) #t]
      [(define-values (id) _)
       ;; These three identifiers are never used
       (memq (syntax-e #'id) (list 'open-output-file/truncate
                                   'call-with-output-file/truncate
                                   'fatal-error))]
      [(#%require _) #t]
      [_ #f]))

  (define (ids->vars ids)
    (foldr (lambda (id rest) #`(varl #,id #,rest))
           #'varsnil
           ids))
  (define (es->el es)
    (foldr (lambda (e es-rest) #`(el #,(kernel->core e) #,es-rest))
           #'esnil
           es))

  (define (kernel->core stx)
    (syntax-parse stx
      #:datum-literals (module
                        [mb #%module-begin]
                        [app #%app]
                        [lam lambda]
                        quote
                        define-values
                        let-values
                        letrec-values
                        if)
      [(module id lang
         (mb form ...+))
       #`(modbegin
          #,(foldr
             (lambda (form rest)
               #`(mf #,(kernel->core form) #,rest))
             #'mfnil
             (filter-not ignored-modform? (syntax->list #'(form ...)))))]
      [(app (~datum call-with-values) (lam () exp) (~datum print-values))
       #`(app call-with-values
              (el #,(kernel->core #'(lambda () exp))
                  (el #,(kernel->core #'(lambda rest
                                          (#%app foldl (lambda (x rest) (#%app print x)) (#%app void) rest)))
                      esnil)))]
      [(app e1 e-rest ...)
       #`(app
          #,(kernel->core #'e1)
          #,(es->el (attribute e-rest)))]
      [(lam x:id e es ...)
       #`(lamrest varsnil x #,(kernel->core #'e) #,(es->el (attribute es)))]
      [(lam (xs ...) e es ...)
       #`(lam #,(ids->vars (attribute xs))
           #,(kernel->core #'e)
           #,(es->el (attribute es)))]
      [(lam (xs ... . rest:id) e es ...)
       #`(lamrest #,(ids->vars (attribute xs)) rest
           #,(kernel->core #'e)
           #,(es->el (attribute es)))]
      [(define-values (id) e)
       #`(define id #,(kernel->core #'e))]
      [((~or (~and let-values (~bind [name 'letvalues]))
             (~and letrec-values (~bind [name 'letrecvalues])))
        ([(xs ...) e] ...)
        e-body0 e-body ...)
       #`(#,(attribute name)
          #,(foldr
             (lambda (ids e rest)
               #`(vbl
                  (vb #,(ids->vars ids) #,(kernel->core e))
                  #,rest))
             #'valuesbindsnil
             (attribute xs)
             (attribute e))
          #,(kernel->core #'e-body0)
          #,(es->el (attribute e-body)))]
      [(if e1 e2 e3)
       #`(if #,(kernel->core #'e1)
             #,(kernel->core #'e2)
             #,(kernel->core #'e3))]
      [(quote #t) #'(quote true)]
      [(quote #f) #'(quote false)]
      [(quote ()) #'(quote nil)]
      [(quote s:id) #'(quote (sym s))]
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
