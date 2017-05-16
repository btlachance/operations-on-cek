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

  ;; A binding is a (binding id type) where the first id is the
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
  ;; - (pattern-presult type (listof binding)) representing the result
  ;;   of succesfully parsing a pattern's syntax
  (struct pattern-presult (type bindings))

  (struct form (type ;; type -- I'm not certain we'll want this but it
                     ;; seems like we will for things like lookup and
                     ;; extend metafunctions
                parse-pattern ;; syntax -> pattern-presult
                parse-template
                compile-pattern
                compile-template
                is-default?)))

(module prims-impl racket
  (require (submod ".." rep) syntax/parse "compile-util.rkt")
  (provide mk-variable mk-number mk-numbers mk-cons)
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
    (form ty parse-cons-pattern #f #f #f #f)))

(module+ test
  (require (submod ".." prims-impl test)))

(module prims racket
  (require (for-syntax (submod ".." prims-impl)))
  (provide variable number numbers cons)
  (define-syntax variable (mk-variable #'variable))
  (define-syntax number (mk-number #'number))
  (define-syntax numbers (mk-numbers #'numbers #'number))
  (define-syntax cons (mk-cons #'cons (list #'variable #'number #'numbers))))

(require 'prims (for-syntax 'rep racket/bool))
(begin-for-syntax
  (define cons-form (syntax-local-value #'cons))
  (define parse-cons-pattern (form-parse-pattern cons-form))
  (if (false? (parse-cons-pattern #'(cons (cons number_1 number_2) number_3)))
      (println "failed")
      (println "success!")))
