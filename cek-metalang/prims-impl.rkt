#lang racket
(require "rep.rkt"
         syntax/parse
         "compile-util.rkt")
(provide mk-variable
         mk-number
         mk-numbers
         mk-cons)
(define (mk-variable ty)
  (define (parse-variable-pattern stx)
    (syntax-parse stx
      [(~var v (pattern-metavar ty))
       (pattern-presult ty (list (binding this-syntax ty)))]
      [_ #f]))
  (form ty parse-variable-pattern #f #f #f #t))
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
       (pattern-presult ty (list (binding this-syntax ty)))]
      [_ #f]))
  (form ty parse-number-pattern #f #f #f #t))
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
       (pattern-presult ty (list (binding this-syntax ty)))]
      [(t0 t ...)
       #:when (free-identifier=? ty #'t0)
       (define number-f (syntax-local-value number-ty))
       (define parse-number-pattern (form-parse-pattern number-f))
       (pattern-presult
        ty
        (for/fold ([bindings '()])
                  ([pattern (attribute t)])
          (define result (parse-number-pattern pattern))
          (append (pattern-presult-bindings result) bindings)))]
      [_ #f]))
  (form ty parse-numbers-pattern #f #f #f #t))

(define (parse-alts-pattern alts pattern)
  (for/or ([f (map syntax-local-value alts)])
    ((form-parse-pattern f) pattern)))

(define (mk-cons ty alts)
  (define alts* (cons ty alts))
  (define (parse-cons-pattern stx)
    (syntax-parse stx
      [(cons t1 t2)
       ;; because I insisted on using Racket's cons identifier as
       ;; the identifier for cons patterns... I can't write cons in
       ;; the template
       (define t1-presult (parse-alts-pattern alts* #'t1))
       (define t2-presult (parse-alts-pattern alts* #'t2))
       (pattern-presult
        ty
        (append (pattern-presult-bindings t1-presult)
                (pattern-presult-bindings t2-presult)))]
      [_ #f]))
  (form ty parse-cons-pattern #f #f #f #f))
