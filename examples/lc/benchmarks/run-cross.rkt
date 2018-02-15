#lang racket
(define PYCKET-BENCH-PATH (make-parameter #f))
(define INTERPRETER-CMD+ARGS (make-parameter '(#f)))
(define COMPILE-TERM-CMD+ARGS (make-parameter '(#f)))

;; A step-result is one of
;; (empty-result)
;; (path string)
;; (error natural)
(struct empty-result ())
(struct path (str))
(struct err (code message))

;; A timings is a (timings symbol natural natural natural).
;; Each natural represents, in order, the number of milliseconds the
;; benchmark spent in
;; - cpu time
;; - gc time
;; - total time
(struct timings (benchmark cpu gc total) #:transparent)

(define (expand-benchmark bench-name racket-source-path)
  (define output-path (make-temporary-file (format "~a-bench-~~a" bench-name)))
  (define raco-path (find-executable-path "raco"))

  (match (with-output-to-file output-path
           (lambda () (system*/exit-code raco-path "expand" racket-source-path))
           #:exists 'truncate)
    [0 (path output-path)]
    [error-status (err error-status (format "raco path was" raco-path))]))

(define (jsonify-benchmark bench-name expanded-path)
  (define output-path (make-temporary-file (format "~a-bench-~~a.json" bench-name)))

  (with-input-from-file expanded-path
    (lambda ()
      (match (with-output-to-file output-path
               (lambda () (apply system*/exit-code (COMPILE-TERM-CMD+ARGS)))
               #:exists 'truncate)
        [0 (path output-path)]
        [error-status (err error-status (format "tried to jsonify with command '~a'" (COMPILE-TERM-CMD+ARGS)))]))))

(define (get-timings port)
  (match (get-output-string port)
    [(regexp #rx"^.*\n([0-9]+)\n([0-9]+)\n([0-9]+)" (list _ cpu gc total))
     (values (string->number cpu)
             (string->number gc)
             (string->number total))]))

(define (run-benchmark name racket-source-path)
  (define expanded-path
    (match (expand-benchmark name racket-source-path)
      [(path exp) exp]
      [(err error-code msg)
       (raise-arguments-error 'run-cross
                              "when expanding benchmark, got an error"
                              "benchmark" name
                              "error code" error-code
                              "error msg" msg)]))

  (define benchmark-json-path
    (match (jsonify-benchmark name expanded-path)
      [(path json) json]
      [(err error-code msg)
       (raise-arguments-error 'run-cross
                              "when jsonifying, got an error"
                              "path" expanded-path
                              "error code" error-code
                              "error msg" msg)]))

  (define sub-output-port (open-output-string (format "~a-output" name)))
  (define timings-output-port (open-output-string (format "~a-timings" name)))
  (define benchmark-json-input-port (open-input-file benchmark-json-path))

  (parameterize ([current-output-port sub-output-port]
                 [current-input-port benchmark-json-input-port]
                 [current-error-port timings-output-port])
    (match (apply system*/exit-code (INTERPRETER-CMD+ARGS))
      [0 (void)]
      [error-code
       (raise-arguments-error 'run-cross
                              "when running benchmark, got an error code"
                              "benchmark" name
                              "error code" error-code
                              "output" (get-output-string sub-output-port))]))

  (define-values (cpu gc total) (get-timings timings-output-port))
  (timings name cpu gc total))


(module+ main
  (define default-benchmarks '(fib mbrot sum nqueens fft perm9 pnpoly
                               triangl diviter paraffins simplex))
  (define benchmarks (make-parameter '()))

  (struct bench-format (before with-timings))
  (define default-format
    (bench-format
     (lambda (name) (pretty-display (format "Running benchmark ~a" name)))
     (lambda (timings) (pretty-display timings))))
  (define pycket-bench
    (bench-format
     (lambda (name) (void))
     (lambda (t)
       (match-define (timings _ cpu gc total) t)
       (pretty-display (format "RESULT-cpu: ~a.0\nRESULT-gc: ~a.0\nRESULT-total: ~a.0"
                               cpu gc total)))))
  (define output-format (make-parameter default-format))

  (command-line
   #:program "run-cross"
   #:once-each
   ["--pycket-bench" "Write output that's compatible with pycket-bench"
                     (output-format pycket-bench)]
   ["--default-bench" "Write output using the built-in format (default)"
                      (output-format default-format)]
   #:multi
   ["--bench" b
              "Run benchmark <b>"
              (benchmarks (cons (string->symbol b) (benchmarks)))])

  (parameterize ([PYCKET-BENCH-PATH (expand-user-path "~/projects/pycket-bench")]
                 [INTERPRETER-CMD+ARGS (list (expand-user-path "~/projects/operations-on-cek/build/interpreter-lc/cek-c"))]
                 [COMPILE-TERM-CMD+ARGS (list (find-executable-path "racket") (expand-user-path "~/projects/operations-on-cek/examples/lc/lc.rkt") "--compile-term")]
                 [error-print-width 4096])
    (for ([b (if (empty? (benchmarks))
                 default-benchmarks
                 (reverse (benchmarks)))])
      (define racket-source-path
        (build-path (PYCKET-BENCH-PATH) "CrossBenchmarks" "racket" (format "~a-nothing.rkt" b)))
      (let ([before (bench-format-before (output-format))]
            [with-timings (bench-format-with-timings (output-format))])
        (before b)
        (with-timings (run-benchmark b racket-source-path))))))
