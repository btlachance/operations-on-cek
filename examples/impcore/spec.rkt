#lang racket
(require "../../cek-metalang/cek-metalang.rkt")
(define-cek impcore
  #:grammar
  ;; TODO figure out why this doesn't work as (term ::= program e)
  (term ::= program deff)
  (program ::= (defs deff program) defsnil)

  (result ::= v vs)
  (vars ::= (varl var vars) varsnil)
  (es ::= (el e es) esnil)
  (vs ::= (vl v vs) vsnil)

  (deff ::= (val var e) (define var vars e) e)
  (e ::= ignore literal var (set var e) (if e e e) (while e e)
     (begin es) (app var es))
  (var ::= variable)
  (literal ::= (quote number))
  (v ::= number (fun vars e))

  (env ::= dummy)
  (envs ::= (eee env env env))

  (k ::= mt (sel e e k) (looptest k) (repeat e e k) (seq es k) (fn vs es vars e k)
     (poplocalenv env k) (ret v k) (bindglobalk var k) (bindlocalk var k)
     (printk k) (progk program k))

  #:control-string term
  #:environment envs
  #:continuation k
  #:initial [program
             -->
             ((defs (define + (varl m (varl n varsnil)) (addimpl m n))
              (defs (define - (varl m (varl n varsnil)) (subimpl m n))
              (defs (define * (varl m (varl n varsnil)) (multimpl m n))
              (defs (define / (varl m (varl n varsnil)) (divimpl m n))
              (defs (define < (varl m (varl n varsnil)) (ltimpl m n))
              (defs (define > (varl m (varl n varsnil)) (gtimpl m n))
              (defs (define = (varl m (varl n varsnil)) (numequalimpl m n))
              (defs (define print (varl n varsnil) (printimpl n))
              (defs (define println (varl n varsnil) (printlnimpl n))
              (defs (define and (varl b (varl c varsnil)) (if b c b))
              (defs (define or (varl b (varl c varsnil)) (if b b c))
              (defs (define not (varl b varsnil) (if b (quote 0) (quote 1)))
              (defs (define <= (varl x (varl y varsnil)) (app not (el (app > (el x (el y esnil))) esnil)))
              (defs (define >= (varl x (varl y varsnil)) (app not (el (app < (el x (el y esnil))) esnil)))
              (defs (define != (varl x (varl y varsnil)) (app not (el (app = (el x (el y esnil))) esnil)))
              (defs (define mod (varl m (varl n varsnil)) (app - (el m (el (app * (el n (el (app / (el m (el n esnil))) esnil))) esnil))))
                program))))))))))))))))
              (eee (emptyenv) (emptyenv) (emptyenv))
              mt)]
  #:final [(defsnil envs mt) --> (quote 0)]

  #:step
  [((defs (val var e_0) program) (eee env_g env_f env_l) k)
   -->
   (e_0 (eee env_g env_f (emptyenv)) (bindglobalk var (progk program k)))]

  [((defs (define var_fname vars_formals e_0) program) (eee env_g env_f0 env_l) k)
   -->
   (program (eee env_g env_f1 env_l) k)
   #:where env_f1 (extend env_f0 (varl var_fname varsnil) (vl (fun vars_formals e_0) vsnil))]

  [((defs e_0 program) envs k)
   -->
   (e_0 (eee env_g env_f (emptyenv)) (bindglobalk it (progk program k)))
   #:where (eee env_g env_f env_l) envs]

  [(literal envs k)
   -->
   (ignore envs (ret number k))
   #:where (quote number) literal]

  [(var envs k)
   -->
   (ignore envs (ret v k))
   #:where (eee env_g env_f env_l) envs
   #:where v (lookup_backtrack env_l var)]

  [(var envs k)
   -->
   (ignore envs (ret v k))
   #:where (eee env_g env_f env_l) envs
   #:where v (lookup env_g var)]

  [((set var e_0) envs k)
   -->
   (e_0 envs (bindlocalk var k))
   #:where (eee env_g env_f env_l) envs
   #:where v (lookup_backtrack env_l var)]

  [((set var e_0) envs k)
   -->
   (e_0 envs (bindglobalk var k))
   #:where (eee env_g env_f env_l) envs
   #:where v (lookup env_g var)]

  [((if e_1 e_2 e_3) envs k)
   -->
   (e_1 envs (sel e_2 e_3 k))]

  [((while e_test e_body) envs k)
   -->
   (e_test envs (looptest (repeat e_test e_body k)))]

  [((begin esnil) envs k)
   -->
   (ignore envs (ret 0 k))]

  [((begin (el e_0 es)) envs k)
   -->
   (e_0 envs (seq es k))]

  [((app var es) envs k)
   -->
   (ignore envs (fn vsnil es vars e_0 k))
   #:where (eee env_g env_f env_l) envs
   #:where (fun vars e_0) (lookup env_f var)]

  [(ignore envs (ret 0 (sel e_then e_else k)))
   -->
   (e_else envs k)]

  [(ignore envs (ret v (sel e_then e_else k)))
   -->
   (e_then envs k)
   #:unless 0 v]

  [(ignore envs (ret v (looptest (repeat e_test e_body k))))
   -->
   (e_body envs (repeat e_test e_body k))
   #:unless 0 v]

  [(ignore envs (ret v (looptest (repeat e_test e_body k))))
   -->
   (ignore envs (ret v k))
   #:where 0 v]

  [(ignore envs (ret v (repeat e_test e_body k)))
   -->
   (e_test envs (looptest (repeat e_test e_body k)))]

  [(ignore envs (ret v (seq esnil k)))
   -->
   (ignore envs (ret v k))]

  [(ignore envs (ret v (seq (el e_0 es) k)))
   -->
   (e_0 envs (seq es k))]

  [(ignore envs (ret v (fn vs es vars e_0 k)))
   -->
   (ignore envs (fn (vl v vs) es vars e_0 k))]

  [(ignore envs (fn vs (el e_next es_rest) vars e_0 k))
   -->
   (e_next envs (fn vs es_rest vars e_0 k))]

  [(ignore envs (fn vs esnil vars e_0 k))
   -->
   (e_0 (eee env_g env_f env_l1) (poplocalenv env_l0 k))
   #:where (eee env_g env_f env_l0) envs
   #:where env_l1 (extend (emptyenv) vars (vsreverse vs))]

  [(ignore envs (ret v (poplocalenv env_l1 k)))
   -->
   (ignore (eee env_g env_f env_l1) (ret v k))
   #:where (eee env_g env_f env_l0) envs]

  [(ignore envs (ret v (bindglobalk var k)))
   -->
   (ignore (eee env_g1 env_f env_l) k)
   #:where (eee env_g0 env_f env_l) envs
   #:where env_g1 (extend env_g0 (varl var varsnil) (vl v vsnil))]

  [(ignore envs (ret v (bindlocalk var k)))
   -->
   (ignore (eee env_g env_f env_l1) k)
   #:where (eee env_g env_f env_l0) envs
   #:where env_l1 (extend env_l0 (varl var varsnil) (vl v vsnil))]

  [(ignore envs (progk program k))
   -->
   (program envs k)])

(module+ main
  (require (only-in (submod ".."impcore) corify)
           syntax/location
           syntax/parse)

  (define (compile-term stx)
    (parameterize ([current-namespace (make-base-empty-namespace)])
      ;; Although I could run programs via racket/DrRacket/racket-mode
      ;; that had a #lang like #lang s-exp (submod ...), I couldn't
      ;; call expand on them. I would get an error like
      ;;
      ;; require: unknown module
      ;;   module name: #<resolved-module-path:(submod "/Users/blachance/projects/operations-on-cek/examples/spec.rkt" impcore)>
      ;;
      ;; So eventually I got to a point where I thought the submod form
      ;; was the problem, where I learned that it's only used for
      ;; relative paths. Then I tried, from a plain Racket repl,
      ;; (require (submod (file "/Users/blachance/.../spec.rkt") impcore))
      ;; and that also gave me errors. So I decided then to ditch the
      ;; submod form entirely since I couldn't even get it to work in 
      ;; plain Racket. That's why there's this impcore.rkt file that
      ;; just does what the impcore submodule below does.

      (namespace-require (quote-module-path ".." impcore))

      (define expanded (expand stx))
      (syntax-parse expanded
        #:literal-sets (kernel-literals)
        [(module name lang
           (#%module-begin mod-config
                           forms ...))
         (corify (attribute forms))])))

  (read-accept-reader #t)
  (read-accept-lang #t)
  (command-line
   #:program "impcore"
   #:once-any
   ["--print-interp" "Print the Python definition of the interpreter"
                     (print-impcore-interp)]
   ["--print-parser" ("Print the Python-based parser that consumes"
                      "JSON representations of impcore terms and"
                      "produces AST nodes")
                     (print-impcore-parser)]
   ["--compile-term" ("Read a impcore term from stdin and print the"
                      "JSON representation of that term to stdout.")
                     (display (impcore-term->json (compile-term (read-syntax))))]
   ["--pretty-print-term" "Like --compile-term, but print before JSON"
                          (pretty-print (syntax->datum (compile-term (read-syntax))))]))

(module impcore racket
  (provide module
           #%module-begin
           #%top
           #%top-interaction
           print
           println
           begin
           + - * / < > =
           (rename-out
            [imp:datum #%datum]
            [imp:app #%app]
            [imp:define define]
            [imp:val val]
            [imp:while while]
            [imp:if if]
            [imp:and and]
            [imp:or or]
            [imp:<= <=]
            [imp:>= >=]
            [imp:!= !=]
            [imp:mod mod]
            [imp:and and]
            [imp:or or]
            [imp:not not]
            [imp:<= <=]
            [imp:>= >=]
            [imp:!= !=]
            [imp:mod mod]
            [set! set])
           corify)
  (require syntax/parse syntax/parse/define)

  (define-literal-set impcore-literals
    (test imp:and imp:or imp:not imp:<= imp:>= imp:!= imp:mod imp:datum
     imp:app imp:define imp:val imp:while imp:if))

  ;; TODO I don't quite handle environments right, e.g. a user can
  ;; write the name of a function and get back its corresponding
  ;; value. They can't do anything with it but it should technically
  ;; be a runtime error.
  ;; TODO And I also don't yet have the right implementation for
  ;; val---it needs to expand to either a set! or a define

  (define (test x) (not (zero? x)))

  (define (imp:and b c) (if (test b) c b))
  (define (imp:or b c) (if (test b) b c))
  (define (imp:not b) (if (test b) 0 1))
  (define (imp:<= m n) (imp:not (> m n)))
  (define (imp:>= m n) (imp:not (< m n)))
  (define (imp:!= m n) (not (= m n)))
  (define (imp:mod m n) (- m (* n (/ m n))))

  (define-simple-macro (imp:datum . num:exact-integer)
    (#%datum . num))
  (define-simple-macro (imp:app fname:id e ...)
    (#%app fname e ...))
  (define-simple-macro (imp:define fname:id (args:id ...) e:expr)
    (define fname (lambda (args ...) e)))
  (define-simple-macro (imp:val name:id e:expr)
    ;; TODO val of an already-defined name needs to be allowed
    (define name e))
  (define-simple-macro (imp:while e1:expr e2:expr)
    (let loop ()
      (if (test e1)
          (begin
            e2
            (loop))
          0)))
  (define-simple-macro (imp:if e1:expr e2:expr e3:expr)
    (if (test e1)
        e2
        e3))

  (define (ids->vars ids)
    (foldr (lambda (var rest) #`(varl #,var #,rest)) #'varsnil ids))
  (define (expressions->es exprs)
    (foldr (lambda (e rest) #`(el #,e #,rest)) #'esnil exprs))

  ;; TODO need to turn set!'s into impcore forms
  ;; TODO b/c of how I print toplevel expressions, need to make sure
  ;;      there's a global binding for it if the program contains a
  ;;       toplevel expression
  (define (corify forms)
    (define (corify-form form)
      (syntax-parse form
        #:literal-sets (kernel-literals impcore-literals)
        #:literals (call-with-values)
        [(define-values (fname) (lambda (arg ...) e))
         #`(define fname #,(ids->vars (attribute arg)) #,(corify-form #'e))]

        [(define-values (id) e)
         #`(val id #,(corify-form #'e))]

        [(quote n)
         this-syntax]

        [(if (#%plain-app test e1) e2 e3)
         ;; I apparently don't understand unsyntax-splicing because I
         ;; tried use it inside of the #`(if ...) and that ended up
         ;; passing something like
         ;;     '(#<syntax:spec.rkt:327:12 if> . #<syntax (n ...))
         ;; to syntax-parse, which then passed that to length.
         #`(if #,@(map corify-form (list #'e1 #'e2 #'e3)))]

        [var:id
         #'var]

        [(#%plain-app call-with-values (#%plain-lambda () e) (~datum print-values))
         (corify-form #'e)]

        [(#%plain-app fname:id e ...)
         #`(app fname #,(expressions->es (map corify-form (attribute e))))]

        [(begin e ...)
         #`(begin #,(expressions->es (map corify-form (attribute e))))]))

    (define (reverse-program p)
      (define (reverse-program-onto p onto)
        (syntax-parse p
          #:datum-literals (defs defsnil)
          [defsnil onto]
          [(defs d p*)
           (reverse-program-onto #'p* #`(defs d #,onto))]))
      (reverse-program-onto p #'defsnil))

    (define seen-toplevel? #f)
    (reverse-program
     (foldl
      ;; We want to add a val definition for the it variable the first
      ;; time we see a toplevel expression, and that means means we
      ;; need to foldl
      (lambda (def rest)
        (define-values (def* rest*)
          (syntax-parse def
            #:datum-literals (val define)
            [(define id (vars ...) e) (values def rest)]
            [(val id e) (values def rest)]
            [e
             (if (not seen-toplevel?)
                 (begin
                   (set! seen-toplevel? #t)
                   (values #'(printlnimpl it) #`(defs (val it e) #,rest)))
                 (values #'(printlnimpl it) #`(defs e #,rest)))]))
        #`(defs #,def* #,rest*))
      #'defsnil
      (map corify-form forms)))))