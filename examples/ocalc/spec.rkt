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

  (v ::=
     (clo l env)
     c
     (obj env))

  (var ::= variable)
  (l ::= (lam var e))
  (c ::=
     number
     add1
     sub1)

  (env ::= dummy)
  (k ::=
     mt
     (arg e env k)
     (op v k)
     (sel e e env k)
     (gethandler e var env k)
     (extendobj v var k)
     (deliver var k)
     (ret v k))

  #:control-string e
  #:environment env
  #:continuation k

  #:initial [e --> (e (emptyenv) mt)]
  #:final
  ;; If the final result of a program can be either a number or a
  ;; procedure and I want to have different final transitions that
  ;; distinguish between those two cases, then I need some kind of
  ;; disjunction for #:final. Right now the only disjunction in the
  ;; metalanguage is through rule sequencing, so that suggests it
  ;; would be nice to allow multiple rules for #:final. I'll work
  ;; around by just calling something in Python land.
  [(ignore env (ret v mt)) --> (quote 0)
   #:with (primprint v)]

  #:step
  [(var env k) --> (ignore env (ret (lookup env var) k))]
  [(l env k) --> (ignore env (ret (clo l env) k))]
  [((quote c) env k) --> (ignore env (ret c k))]
  [(emptyobj env k) --> (ignore env (ret (obj (emptyenv)) k))]
  [((app e_op e_arg) env k) --> (e_op env (arg e_arg env k))]
  [((ifz e_test e_then e_else) env k) --> (e_test env (sel e_then e_else env k))]
  [((respond e_obj var e_handler) env k) --> (e_obj env (gethandler e_handler var env k))]
  [((send e_obj var) env k) --> (e_obj env (deliver var k))]

  [(ignore env_0 (ret v (arg e env k))) --> (e env (op v k))]
  [(ignore env (ret v (op add1 k))) --> (ignore env (ret (primadd v 1) k))]
  [(ignore env (ret v (op sub1 k))) --> (ignore env (ret (primsub v 1) k))]
  [(ignore env_0 (ret v (op (clo (lam var e_body) env_1) k)))
   -->
   (e_body env k)
   #:where env (extend1 env_1 var v)]
  [(ignore env_0 (ret 0 (sel e_then e_else env k))) --> (e_then env k)]
  [(ignore env_0 (ret v (sel e_then e_else env k))) --> (e_else env k)
   #:unless 0 v]
  [(ignore env_0 (ret v (gethandler e var env k))) --> (e env (extendobj v var k))]
  [(ignore env_0 (ret v_handler (extendobj (obj env) var k)))
   -->
   (ignore env_0 (ret v_extended k))
   #:where v_extended (obj (extend1 env var v_handler))]
  [(ignore env_0 (ret v_obj (deliver var k))) --> (ignore env_0 (ret v_obj (op v k)))
   #:where (obj env) v_obj
   #:where v (lookup env var)])

(module+ main
  (command-line
   #:program "ocalc"
   #:once-any
   ["--print-interp" "Print the Python definition of the interpreter"
                     (print-ocalc-interp)]
   ["--print-parser" ("Print the Python-based parser that consumes"
                      "JSON representations of ocalc terms and"
                      "produces AST nodes")
                     (print-ocalc-parser)]
   ["--compile-term" ("Read a ocalc term from stdin and print the"
                      "JSON representation of that term to stdout.")
                     (display (ocalc-term->json (read-syntax)))]
   ["--pretty-print-term" "Like --compile-term, but print before JSON"
                          (pretty-print (syntax->datum (read-syntax)))]))

(module+ test
  (require rackunit syntax/location)
  (define-values (racket/p python/p make/p)
    (apply values (map find-executable-path '("racket" "python" "make"))))
  (define ROOT (build-path (syntax-source-directory #'here) 'up 'up))
  (define main.py
    ;; make doesn't like the absolute path in PY_MAIN so we need to
    ;; give it a relative path
    (build-path "build" "interpreter-ocalc" "main.py"))
  (define PY_MAIN (build-path ROOT main.py))

  (parameterize ([current-directory ROOT])
    (check-true (system* (find-executable-path "make") "--quiet" main.py)))

  (define (json-of test)
    (define stx (datum->syntax #f test))
    (ocalc-term->json stx))

  (define (roundtrip-of test)
    (with-output-to-string
      (lambda ()
        (parameterize ([current-input-port (open-input-string (~s test))])
          (system* racket/p (quote-source-file) "--compile-term")))))

  (define-syntax-rule (check-parse exp)
    (begin
      (define t 'exp)
      (check-equal? (roundtrip-of t) (json-of t))))

  (check-parse (quote 10))
  (check-parse (send x x))
  (check-parse (respond emptyobj x (lam x x)))
  (check-parse (app (lam x x) (lam x x)))
  (check-parse (ifz x y x))

  (define (eval exp)
    (define err (open-output-string))
    (parameterize ([current-input-port (open-input-string (json-of exp))]
                   [current-error-port err])
      (define out (with-output-to-string (lambda () (system* python/p PY_MAIN))))
      (regexp-replace #rx"\n$" out "")))
  (define-syntax-rule (check-evaluates-to exp s) (check-equal? (eval 'exp) s))

  (check-evaluates-to (quote 10)
                      "10")
  (check-evaluates-to (app (quote add1) (quote 10))
                      "11")
  (check-evaluates-to (lam x x)
                      "<procedure>")
  (check-evaluates-to emptyobj
                      "<object>")
  (check-evaluates-to (respond emptyobj x (lam self self))
                      "<object>")
  (check-evaluates-to (send (respond emptyobj y (lam self (app (quote sub1) (quote 42))))
                            y)
                      "41"))
