#lang racket
(module rep racket
  (require rackunit)
  (provide
   (struct-out language)
   (struct-out binding)
   binding-equal?
   check-binds?
   check-not-binds?
   (struct-out pattern-presult)
   (struct-out form))
  ;; A language is a (language (listof form))
  (struct language (forms))

  ;; A type is an id with a transformer binding. The value for that
  ;; binding must be a form

  ;;  binding is a (binding id type) where the first id is the
  ;; identifier being bound and the second id represents the
  ;; identifier's type
  (struct binding (id type))
  (define (binding-equal? b1 b2)
    (and (free-identifier=? (binding-id b1) (binding-id b2))
         (free-identifier=? (binding-type b1) (binding-type b2))))
  (define-binary-check (check-binds? actual expected)
    (define result actual)
    (and (pattern-presult? result)
         (memf (lambda (b) (binding-equal? b expected))
               (pattern-presult-bindings actual))))
  (define-binary-check (check-not-binds? actual unexpected)
    (define result actual)
    (or (false? result)
        (not (memf (lambda (b) (binding-equal? b unexpected))
                   (pattern-presult-bindings actual)))))

  ;; A pattern-presult is one of
  ;; - #f
  ;; - (pattern-presult (listof binding)) representing the result of
  ;; succesfully parsing a pattern's syntax
  (struct pattern-presult (bindings))


  (struct form (parse-pattern ;; syntax -> pattern-presult
                parse-template
                compile-pattern
                compile-template
                is-default?)))

(module prims-impl racket
  (require (submod ".." rep) syntax/parse "compile-util.rkt")
  (provide mk-variable mk-number mk-numbers)
  (define (mk-variable ty)
    (define (parse-variable-pattern stx)
      (syntax-parse stx
        [(~var v (pattern-metavar ty))
         (pattern-presult (list (binding this-syntax ty)))]
        [_ #f]))
    (form parse-variable-pattern #f #f #f #t))
  (module+ test
    (require rackunit)
    ;; These tests don't really live up to the contract on mk-variable
    ;; because we didn't first create a transformer binding. Ditto for
    ;; mk-number.
    (define test-var (mk-variable #'variable))
    (define parse-variable-pattern (form-parse-pattern test-var))
    (check-binds? (parse-variable-pattern #'variable)
                  (binding #'variable #'variable))
    (check-binds? (parse-variable-pattern #'variable_1)
                  (binding #'variable_1 #'variable))
    (check-not-binds? (parse-variable-pattern #'number)
                      (binding #'number #'variable)))

  (define (mk-number ty)
    (define (parse-number-pattern stx)
      (syntax-parse stx
        [(~var n (pattern-metavar ty))
         (pattern-presult (list (binding this-syntax ty)))]
        [_ #f]))
    (form parse-number-pattern #f #f #f #t))
  (module+ test
    (define test-num (mk-number #'number))
    (define parse-number-pattern (form-parse-pattern test-num))
    (check-binds? (parse-number-pattern #'number)
                  (binding #'number #'number))
    (check-not-binds? (parse-number-pattern #'variable_1)
                      (binding #'variable_1 #'number)))

  (define (mk-numbers ty number-ty)
    (define (parse-numbers-pattern stx)
      (syntax-parse stx
        [(~var n (pattern-metavar ty))
         (pattern-presult (list (binding this-syntax ty)))]
        [(t0 t ...)
         #:when (free-identifier=? ty #'t0)
         (define number-f (syntax-local-value number-ty))
         (define parse-number-pattern (form-parse-pattern number-f))
         (andmap parse-number-pattern (attribute t))]
        [_ #f]))
    (form parse-numbers-pattern #f #f #f #t)))

(module+ test
  (require (submod ".." prims-impl test)))

(module prims racket
  (require (for-syntax (submod ".." prims-impl)))
  (provide variable number numbers)
  (define-syntax variable (mk-variable #'variable))
  (define-syntax number (mk-number #'number))
  (define-syntax numbers (mk-numbers #'numbers #'number)))

(require 'prims (for-syntax 'rep racket/bool))
(begin-for-syntax
  (define numbers-form (syntax-local-value #'numbers))
  (define parse-numbers-pattern (form-parse-pattern numbers-form))
  (if (false? (parse-numbers-pattern #'(numbers numbers number_1)))
      (println "failed")
      (println "success!")))
