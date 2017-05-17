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
  (define (bound-as-variable? id bindings)
    (memf (lambda (b) (binding-equal? b (binding id ty))) bindings))
  (define (parse-variable-template stx bindings)
    (syntax-parse stx
      [(~and (~var v (pattern-metavar ty))
             ~!
             (~fail #:unless (bound-as-variable? #'v bindings)
                    (format "Expected a bound variable of type '~a'" (syntax-e ty))))
       #t]
      ;; For now this simple form for variable templates will do. If
      ;; all nonterminals have a transformer binding, then we can
      ;; prevent some likely-buggy forms of overlap by seeing if the
      ;; identifier has a form in its transformer binding and
      ;; rejecting if it does
      [id:identifier
       #t]
      [_ #f]))
  (form ty parse-variable-pattern parse-variable-template #f #f (lambda () #t)))
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
                    (binding #'number #'variable))

  (define p-var-template (form-parse-template test-var))
  (check-true (p-var-template #'variable_3 (list (binding #'variable_3 #'variable))))
  (check-exn exn:fail:syntax? (thunk (p-var-template #'variable_4 (list (binding #'variable #'variable))))))

(define (mk-number ty)
  (define (parse-number-pattern stx)
    (syntax-parse stx
      [(~var n (pattern-metavar ty))
       (pattern-presult ty (list (binding this-syntax ty)))]
      [_ #f]))
  (define (bound-as-number? id bindings)
    (memf (lambda (b) (binding-equal? b (binding id ty))) bindings))
  (define (parse-number-template stx bindings)
    (syntax-parse stx
      [(~and (~var n (pattern-metavar ty))
             ~!
             (~fail #:unless (bound-as-number? #'n bindings)
                    (format "Expected a bound variable of type '~a'" (syntax-e ty))))
       #t]
      [:number #t]
      [_ #f]))
  (form ty parse-number-pattern parse-number-template #f #f (lambda () #f)))
(module+ test
  (define test-num (mk-number #'number))
  (define parse-number-pattern (form-parse-pattern test-num))

  (check-binds? (parse-number-pattern #'number)
                (binding #'number #'number))
  (check-not-binds? (parse-number-pattern #'variable_1)
                    (binding #'variable_1 #'number))

  (define p-num-template (form-parse-template test-num))
  (check-true (p-num-template #'number_2
                              (list (binding #'number_2 #'number))))
  (check-true (p-num-template #'10 (list)))
  (check-false (p-num-template #'"hello" (list)))
  (check-false (p-num-template #'variable (list)))
  (check-exn exn:fail:syntax? (thunk (p-num-template #'number (list)))))

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
  (define (parse-numbers-template stx bindings)
    (syntax-parse stx
      [(t0 t ...)
       #:when (free-identifier=? ty #'t0)
       (define number-f (syntax-local-value number-ty))
       (define parse-number-template (form-parse-template number-f))
       (for/and ([template (attribute t)])
         (parse-number-template template bindings))]
      ;; TODO run the cons example without this default case. You get
      ;; an error message that I was not expecting...
      [_ #f]))
  (form ty parse-numbers-pattern parse-numbers-template
        #f #f (lambda () #f)))

(define (parse-alts-pattern alts pattern)
  (for/or ([f (map syntax-local-value alts)])
    ((form-parse-pattern f) pattern)))

(define (parse-alts-template alts template bindings)
  (define forms* (map syntax-local-value alts))
  (define (default-form? f) ((form-is-default? f))) ;; yes, ((parens))
  (define-values (forms default-forms)
    (partition (compose not default-form?) forms*))

  (for/or ([f (in-sequences forms default-forms)])
    ((form-parse-template f) template bindings)))

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
  (define (parse-cons-template stx bindings)
    (syntax-parse stx
      [(cons t1 t2)
       #:with #t (parse-alts-template alts* #'t1 bindings)
       #:with #t (parse-alts-template alts* #'t2 bindings)
       #t]
      [_ #f]))
  (form ty parse-cons-pattern parse-cons-template #f #f (lambda () #f)))
