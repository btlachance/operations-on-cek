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
  (envinfo ::= infoempty (info vars envinfo))
  (callingapp ::= nocallingapp (ca a))

  (e ::= var l a
     (quote c) (if e e e) (letvalues valuesbinds e es) (letrecvalues valuesbinds e es)
     (values var) ignore
     (car e) (cdr e) (nullp e) (mkcons e e) (apply e e) (mkvoid) (cwv var var) (begin e es))
  (valuesbind ::= (vb vars e))
  (binding ::= (bp))
  (bp ::= (p var e))
  (var ::= variable)
  (l ::= (lam vars e es) (lamrest vars e es))
  (a ::= (app e es) (appinfo e es envinfo))
  (v ::= (clo l env) c (cons v v) undefinedv voidv)
  (c ::= nil true false number string (sym var))

  (env ::= dummy)

  (config ::= (conf term env k))
  (k ::= modk expk mt)
  (modk ::= (binddefs modforms vars vs modforms k))
  (expk ::= (fn vs es env envinfo callingapp k) (sel e e env k) (ret result k)
        (cark k) (cdrk k) (nullk k) (consr e env k) (pair v k) (getargs e env k) (applyk v k)
        (evaldefs modform modforms env k) (bindvaluesk env vars valuesbinds env es k)
        (bindrec valuesbinds k) (bindvarscells vars k) (evalrec valuesbinds es k) (setcellsk vars env expk)
        (cwvk var env k) (expsk env es k) extensionk)
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
               (mf (define / (lam (varl m (varl n varsnil)) (divimpl m n) esnil))
               (mf (define box (lam (varl b varsnil) (boximpl b) esnil))
               (mf (define unbox  (lam (varl b varsnil) (unboximpl b) esnil))
               (mf (define set-box! (lam (varl b (varl val varsnil)) (setboximpl b val) esnil))
               (mf (define cons (lam (varl val1 (varl val2 varsnil)) (mkcons val1 val2) esnil))
               (mf (define car (lam (varl val varsnil) (car val) esnil))
               (mf (define cdr (lam (varl val varsnil) (cdr val) esnil))
               (mf (define null? (lam (varl val varsnil) (nullp val) esnil))
               (mf (define null (quote nil))
               (mf (define foldl (lam (varl fn (varl init (varl xs varsnil)))
                                   (if (app null? (el xs esnil))
                                       init
                                       (app foldl (el fn (el (app fn (el (app car (el xs esnil)) (el init esnil))) (el (app cdr (el xs esnil)) esnil))))) esnil))
               (mf (define foldr (lam (varl fn (varl init (varl xs varsnil)))
                                   (if (app null? (el xs esnil))
                                       init
                                       (app fn (el (app car (el xs esnil)) (el (app foldr (el fn (el init (el (app cdr (el xs esnil)) esnil)))) esnil))))
                                   esnil))
               (mf (define list (lamrest (varl args varsnil) (app foldr (el cons (el null (el args esnil)))) esnil))
               (mf (define append (lamrest (varl ls varsnil)
                                    (app foldl (el (lam (varl l0 (varl rest varsnil))
                                                     (app foldr (el cons (el l0 (el rest esnil))))
                                                     esnil)
                                                   (el null
                                                       (el ls esnil))))
                                    esnil))
               (mf (define print (lam (varl val varsnil) (printimpl val) esnil))
               (mf (define = (lam (varl m (varl n varsnil)) (numequalimpl m n) esnil))
               (mf (define < (lam (varl m (varl n varsnil)) (ltimpl m n) esnil))
               (mf (define > (lam (varl m (varl n varsnil)) (gtimpl m n) esnil))
               (mf (define <= (lam (varl m (varl n varsnil)) (lteqimpl m n) esnil))
               (mf (define >= (lam (varl m (varl n varsnil)) (gteqimpl m n) esnil))
               (mf (define equal? (lam (varl v1 (varl v2 varsnil)) (eqlimpl v1 v2) esnil))
               (mf (define apply (lam (varl fun (varl args varsnil)) (apply fun args) esnil))
               (mf (define vector (lamrest (varl args varsnil) (vectorimpl args) esnil))
               (mf (define vector-ref (lam (varl vec (varl pos varsnil)) (vecrefimpl vec pos) esnil))
               (mf (define vector-set! (lam (varl vec (varl pos (varl new-v varsnil)))
                                         (vecsetimpl vec pos new-v)
                                         esnil))
               (mf (define vector-length (lam (varl vec varsnil) (veclengthimpl vec) esnil))
               (mf (define make-vector (lamrest (varl size (varl args varsnil))
                                         (letvalues (vbl (vb (varl val varsnil)
                                                             (if (app null? (el args esnil))
                                                                 (quote 0)
                                                                 (app car (el args esnil))))
                                                         valuesbindsnil)
                                                    (makevectorimpl size val)
                                                    esnil)
                                         esnil))
               (mf (define current-command-line-arguments (lam varsnil (app vector esnil) esnil))
               (mf (define void (lamrest (varl args varsnil) (mkvoid) esnil))
               (mf (define symbol? (lam (varl s varsnil) (issymbolimpl s) esnil))
               (mf (define newline (lam varsnil
                                     (app fprintf (el (app current-output-port esnil) (el (quote "\n") esnil)))
                                     esnil))
               (mf (define time-apply (lam (varl proc (varl lst varsnil)) (timeapplyimpl proc lst) esnil))
               (mf (define current-seconds (lam varsnil (currentsecondsimpl) esnil))
               (mf (define current-error-port (lam varsnil (currenterrorportimpl) esnil))
               (mf (define current-output-port (lam varsnil (currentoutputportimpl) esnil))
               (mf (define fprintf (lamrest (varl out (varl form (varl vals varsnil)))
                                     (fprintfimpl out form vals)
                                     esnil))
               (mf (define not (lam (varl val varsnil)
                                 (if val
                                     (quote false)
                                     (quote true))
                                 esnil))
               (mf (define values (lamrest (varl args varsnil) (values args) esnil))
               (mf (define call-with-values (lam (varl gen (varl recv varsnil)) (cwv gen recv) esnil))
               (mf (define exit (lamrest (varl args varsnil)
                                  (letvalues (vbl (vb (varl exitv varsnil)
                                                      (if (app null? (el args esnil))
                                                          (quote true)
                                                          (app car (el args esnil))))
                                                  valuesbindsnil)
                                             (exitimpl exitv)
                                             esnil)
                                  esnil))
               (mf (define exact->inexact (lam (varl m varsnil) (exacttoinexactimpl m) esnil))
               (mf (define exact-integer? (lam (varl val varsnil) (exactintegerp val) esnil))
               (mf (define inexact? (lam (varl val varsnil) (inexactp val) esnil))
               (mf (define quotient (lam (varl m (varl n varsnil)) (quotientimpl m n) esnil))
               (mf (define sin (lam (varl n varsnil) (sinimpl n) esnil))
               ;; Update the list in corify/lc-term->json
                 modforms)))))))))))))))))))))))))))))))))))))))))))))))))))
              (emptyenv)
              mt)]
  #:final [(ignore env_0 (ret v mt)) --> ignore]
  #:step
  ;; IDK what to do when the module body is empty... I don't yet have
  ;; a void value, but I guess that's what I need.
  [((modbegin modforms) env k)
   -->
   (ignore env (binddefs modforms varsnil vsnil modforms k))
   #:where (mf modform_1 modforms_1) modforms]

  [(ignore env (binddefs (mf modform modforms) vars vs modforms_toeval k))
   -->
   (modform env (binddefs modforms vars vs modforms_toeval k))]
  [(ignore env_0 (binddefs mfnil vars_rev vs_rev (mf modform_toeval modforms_toeval) k))
   -->
   (modform_toeval env_1 (evaldefs modform_toeval modforms_toeval env_1 k))
   #:where vars (varsreverse vars_rev)
   #:where vs (vsreverse vs_rev)
   #:where env_1 (extendtoplevel env_0 vars vs)]
  [(ignore env (binddefs mfnil varsnil vsnil mfnil k))
   ;; This rule shouldn't happen since modbegin checks that the module
   ;; body is nonempty
   -->
   (ignore env k)]

  [(gtopform env (binddefs modforms_tobind vars vs modforms_toeval k))
   -->
   (ignore env (binddefs modforms_tobind (varl var vars) (vl v vs) modforms_toeval k))
   #:where (define var e_0) gtopform
   #:where v (mkpromotablecell undefinedv)]
  [(e_0 env (binddefs modforms_tobind vars vs modforms_toeval k))
   -->
   (ignore env (binddefs modforms_tobind vars vs modforms_toeval k))]

  [((define var e_0) env (evaldefs modform modforms env_0 k))
   -->
   (e_0 env_0 (evaldefs modform modforms env_0 k))]
  [(ignore env (ret v (evaldefs (define var e_0) (mf modform modforms) env_0 k)))
   -->
   (modform env_0 (evaldefs modform modforms env_0 k))
   #:where v_ignore (setcell var env_0 v)]

  ;; XXX Why doesn't this set var's cell like the rule when modforms
  ;; isn't empty?
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
  [(a env expk) --> (e_1 env (fn vsnil es env infoempty (ca a) expk))
   #:where (app e_1 es) a]
  [(a env expk) --> (e_1 env (fn vsnil es env envinfo (ca a) expk))
   #:where (appinfo e_1 es envinfo) a]
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
  [((begin e_0 es) env expk) --> (ignore env (expsk env (el e_0 es) expk))]

  [(ignore env_0 (ret v (fn vs es env envinfo callingapp expk))) --> (ignore env_0 (fn (vl v vs) (promotees es) env envinfo callingapp expk))]
  [(ignore env_0 (fn vs (el e es) env envinfo callingapp expk)) --> (e env (fn vs es env envinfo callingapp expk))]
  [(ignore env_0 expk_fn) --> (ignore env_0 (expsk env (el e es) expk))
   #:where (fn vs esnil env_1 envinfo callingapp expk) expk_fn
   #:where (vl v vs_args) (vsreverse vs)
   #:where (clo l env_clo) (trypromote v)
   #:where (lam vars e es) l
   #:where env_spec (env_for_call env_clo envinfo env_1)
   #:where e_ignore (register_call l callingapp expk)
   #:where env (extend env_spec vars vs_args)]
  [(ignore env_0 expk_fn) --> (ignore env_0 (expsk env (el e es) expk))
   #:where (fn vs esnil env_1 envinfo callingapp expk) expk_fn
   #:where (vl v vs_args) (vsreverse vs)
   #:where (clo l env_clo) v
   #:where (lamrest vars e es) l
   #:where env_spec (env_for_call env_clo envinfo env_1)
   #:where e_ignore (register_call l callingapp expk)
   #:where env (extendrest env_spec vars vs_args)]
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
  [(ignore env (ret result (bindvaluesk env_arg vars valuesbinds env_acc0 es expk)))
   -->
   (ignore env (bindvaluesk env_arg varsnil valuesbinds env_acc1 es expk))
   #:where env_acc1 (extend env_acc0 vars result)]

  [(ignore env (bindrec valuesbindsnil expk)) --> (ignore env expk)]
  [(ignore env (bindrec (vbl (vb vars e) valuesbinds) expk))
   -->
   (ignore env (bindvarscells vars (bindrec valuesbinds expk)))]
  [(ignore env (bindvarscells vars expk)) --> (ignore env_1 expk)
   #:where env_1 (extendcells env vars)]
  [(ignore env (evalrec valuesbindsnil es expk)) --> (ignore env (expsk env es expk))]
  [(ignore env (evalrec (vbl (vb vars e) valuesbinds) es expk))
   -->
   (e env (setcellsk vars env (evalrec valuesbinds es expk)))]
  [(ignore env_0 (ret result (setcellsk vars env expk))) --> (ignore env expk)
   #:where v_ignore (setcells vars env result)]

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
  [(ignore env_0 (ret v (applyk (clo (lamrest vars e es) env) expk))) --> (ignore env_0 (expsk (extendrest env vars vs) (el e es) expk))
   #:where vs (vlisttovs v)]
  [(ignore env_0 (ret v (cwvk var_recv env expk))) --> (ignore env_0 (fn (vl v (vl v_recv vsnil)) esnil env_0 infoempty nocallingapp expk))
   #:where v_recv (lookup env var_recv)]
  [(ignore env_0 (ret vs_vals (cwvk var_recv env expk))) --> (ignore env_0 (fn vs esnil env_0 infoempty nocallingapp expk))
   #:where v_recv (lookup env var_recv)
   #:where vs (vsreverse (vl v_recv vs_vals))]
  [(ignore env_0 (ret result k)) --> (e env k)
   #:where extensionk k
   #:where (conf e env k) (docontinuation k result)])

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
  (define (vars->ids vars0)
    (syntax-parse vars0
      [(~literal varsnil) '()]
      [(varl id vars)
       (cons #'id (vars->ids #'vars))]))
  (define (es->el es envinfo)
    (foldr (lambda (e es-rest) #`(el #,(kernel->core e envinfo) #,es-rest))
           #'esnil
           es))

  (define (modforms-ids stxs)
    (for/fold ([ids '()])
              ([modform-stx (reverse stxs)])
      (syntax-parse modform-stx
        #:datum-literals (define-values)
        [(define-values (id) e)
         (cons #'id ids)]
        [_ ids])))

  (define empty-info #'infoempty)
  (define (make-info vars prev-info)
    #`(info #,vars #,prev-info))
  (define (info-vars info)
    (syntax-parse info
      #:datum-literals (info infoempty)
      [infoempty (raise-arguments-error
                  'info-vars
                  "infoempty has no vars")]
      [(info vars _) (attribute vars)]))

  ;; The way I implemented letrec isn't the obvious way: instead of
  ;; initializing a single environment with all of the locations for
  ;; all of the letrec bound variables, I construct an environment for
  ;; each (xs ...). It seemed like the simpler thing to specify at the
  ;; time. This is also (maybe conveniently) the same way that I
  ;; implemented the environments for let, so we can generate the
  ;; environment info from the same function.
  (define (letx-body-info xss prev-info)
    (foldl
     (lambda (xs rest-info) (make-info (ids->vars xs) rest-info))
     prev-info
     xss))

  (define (kernel->core stx envinfo)
    (syntax-parse stx
      #:datum-literals (module
                        [mb #%module-begin]
                        [app #%app]
                        [lam lambda]
                        quote
                        define-values
                        let-values
                        letrec-values
                        begin
                        if)
      [(module id lang
         (mb form ...+))
       ;; INVARIANT: Assumes the identifiers in envinfo are going to
       ;; be bound in the same environment as the one created for the
       ;; modbegin but before the ones in the modbegin
       (define basis-ids (vars->ids (info-vars envinfo)))
       (define forms (filter-not ignored-modform? (attribute form)))
       (define forms-vars (ids->vars (append basis-ids (modforms-ids forms))))
       (define newinfo (make-info forms-vars envinfo))
       #`(modbegin
          #,(foldr
             (lambda (form rest)
               #`(mf #,(kernel->core form newinfo) #,rest))
             #'mfnil
             forms))]
      [(app (~datum call-with-values) (lam () exp) (~datum print-values))
       #`(app call-with-values
              (el #,(kernel->core #'(lambda () exp) envinfo)
                  (el #,(kernel->core #'(lambda rest
                                          (#%app foldl (lambda (x rest) (#%app print x)) (#%app void) rest))
                                      envinfo)
                      esnil)))]
      [(app e1 e-rest ...)
       #`(appinfo
          #,(kernel->core #'e1 envinfo)
          #,(es->el (attribute e-rest) envinfo)
          #,envinfo)]
      [(lam x:id e es ...)
       (define vars (ids->vars (list #'x)))
       (define newinfo (make-info vars envinfo))
       #`(lamrest #,vars #,(kernel->core #'e newinfo) #,(es->el (attribute es) newinfo))]
      [(lam (xs ...) e es ...)
       (define vars (ids->vars (attribute xs)))
       (define newinfo
         (if (null? (attribute xs))
             envinfo
             (make-info vars envinfo)))
       #`(lam #,vars
           #,(kernel->core #'e newinfo)
           #,(es->el (attribute es) newinfo))]
      [(lam (xs ... . rest:id) e es ...)
       (define vars (ids->vars (append (attribute xs) (list #'rest))))
       (define newinfo (make-info vars envinfo))
       #`(lamrest #,vars
           #,(kernel->core #'e newinfo)
           #,(es->el (attribute es) newinfo))]
      [(define-values (id) e)
       ;; Don't create a new envinfo; define-values only appears in a
       ;; modbegin and we create an envinfo that accounts for all of
       ;; the modbegin-bound variables at the modbegin site
       #`(define id #,(kernel->core #'e envinfo))]
      [((~or (~and let-values (~bind [name 'letvalues] [type 'seq]))
             (~and letrec-values (~bind [name 'letrecvalues] [type 'rec])))
        ([(xs ...) e] ...)
        e-body0 e-body ...)

       (define body-info (letx-body-info (attribute xs) envinfo))
       (define rhss-info
         (match (attribute type)
           ['seq envinfo]
           ['rec body-info]))

       #`(#,(attribute name)
          #,(foldr
             (lambda (ids e rest)
               #`(vbl
                  (vb #,(ids->vars ids) #,(kernel->core e rhss-info))
                  #,rest))
             #'valuesbindsnil
             (attribute xs)
             (attribute e))
          #,(kernel->core #'e-body0 body-info)
          #,(es->el (attribute e-body) body-info))]
      [(begin e es ...)
       #`(begin #,(kernel->core #'e envinfo) #,(es->el (attribute es) envinfo))]
      [(if e1 e2 e3)
       #`(if #,(kernel->core #'e1 envinfo)
             #,(kernel->core #'e2 envinfo)
             #,(kernel->core #'e3 envinfo))]
      [(quote #t) #'(quote true)]
      [(quote #f) #'(quote false)]
      [(quote ()) #'(quote nil)]
      [(quote s:id) #'(quote (sym s))]
      [_ this-syntax]))

  (define (corify/lc-term->json stx)
    #;(pretty-print (syntax->datum stx) (current-error-port))
    #;(pretty-print (syntax->datum (kernel->core stx empty-info)) (current-error-port))
    (define names-in-basis
      #'(;; Update these whenever the basis changes
         zero? add1 sub1 + - * / box unbox set-box! cons car cdr null?
         null foldl foldr list append print = < > <= >= equal? apply
         vector vector-ref vector-set! vector-length make-vector
         current-command-line-arguments void symbol? newline time-apply
         current-seconds current-error-port current-output-port fprintf
         not values call-with-values exit exact->inexact exact-integer?
         inexact? quotient sin))
    (define basis-info (make-info (ids->vars (syntax->list names-in-basis)) empty-info))
    (define core (kernel->core stx basis-info))
    (lc-term->json core))

  (command-line
   #:program "lc"
   #:once-any
   ["--print-interp" "Print the Python definition of the interpreter"
                     (print-lc-interp)]
   ["--print-parser" ("Print the Python-based parser that consumes"
                      "JSON representations of lc terms and produces"
                      "AST nodes")
                     (print-lc-parser)]
   ["--compile-term" ("Read a lc term from stdin and print the JSON"
                      "representation of that term to stdout.")
                     (pretty-display (corify/lc-term->json (read-syntax)))]))
