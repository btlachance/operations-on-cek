#lang racket
(require (file "~/projects/operations-on-cek/cek-metalang/cek-metalang.rkt"))

(define-cek tl

  #:grammar
  (e ::=
     var
     (quote number)
     (app op e e)
     (let var e e)
     (ifz e e e)
     ignore)
  (op ::= plus minus)
  (var ::= variable)

  (v ::= number)

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
  #:final
  [(ignore env (ret v mt)) --> (quote 0)
   #:with (primprint v)]

  #:step
  [(var env k) --> (ignore env (ret (lookup env var) k))]

  [((quote number) env k) --> (ignore env (ret number k))]

  
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

(module+ main
  (command-line
   #:program "tl"
   #:once-any
   ["--print-interp" "Print the Python definition of the interpreter"
                     (print-tl-interp)]
   ["--print-parser" ("Print the Python-based parser that consumes"
                      "JSON representations of tl terms and"
                      "produces AST nodes")
                     (print-tl-parser)]
   ["--compile-term" ("Read a tl term from stdin and print the"
                      "JSON representation of that term to stdout.")
                     (display (tl-term->json (read-syntax)))]
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
    (build-path "build" "interpreter-tl" "main.py"))
  (define PY_MAIN (build-path ROOT main.py))

  (parameterize ([current-directory ROOT])
    (check-true (system* (find-executable-path "make") "--quiet" main.py)))

  (define (json-of test)
    (define stx (datum->syntax #f test))
    (tl-term->json stx))

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
  (check-parse (app plus x x))
  (check-parse (let x (quote 0) x))
  (check-parse (ifz (quote 0) x y))

  (define (eval exp)
    (define err (open-output-string))
    (parameterize ([current-input-port (open-input-string (json-of exp))]
                   [current-error-port err])
      (define out (with-output-to-string (lambda () (system* python/p PY_MAIN))))
      (regexp-replace #rx"\n$" out "")))
  (define-syntax-rule (check-evaluates-to exp s) (check-equal? (eval 'exp) s))

  (check-evaluates-to (quote 42)
                      "42")
  (check-evaluates-to (let x (quote 0) (let x (quote 1) x))
                      "1")
  (check-evaluates-to (app plus (quote 21) (quote 21))
                      "42")
  (check-evaluates-to (app minus
                           (quote 10)
                           (ifz (quote 0) (quote -5) (quote 5)))
                      "15"))
