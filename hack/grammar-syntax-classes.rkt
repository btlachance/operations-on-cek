#lang racket
;; (e ::= var v (e e))
;; (var ::= x y z)
;; (v ::= (lam var e))
(module lang-info racket
  (provide (all-defined-out))
  (begin-for-syntax
    (struct metavar ())
    (struct literal ())
    (struct combination ()))

  ;; step 1: generate bindings for the metavariables

  ;; once we know what the metavariables are, then we can distinguish
  ;; literals (e.g. x y z) from references to metavariables
  (define-syntax e (metavar))
  (define-syntax var (metavar))
  (define-syntax v (metavar))
  
  ;; step 2: for each production, generate bindings for each form that
  ;; isn't already defined then generate a pattern for this production

  ;; To generate the bindings we need to know whether its a literal or
  ;; a combination. Because all metavariables have been defined up
  ;; front, we can tell what each combination is made up of and
  ;; connect the combination with its constituent pieces e.g. we know
  ;; app consists of two expressions
  (define-syntax app (combination))

  ;; to generate the pattern for a production, conjoin the patterns
  ;; that we just generated for each of the constituent forms

  (define-syntax x (literal))
  (define-syntax y (literal))
  (define-syntax z (literal))

  (define-syntax lam (combination)))

(require 'lang-info syntax/parse)
(module+ test
  (require
   rackunit
   (for-syntax syntax/parse))

  (define foo 5)
  (check-true (matches-metavar? #'foo #'foo))
  (check-true (matches-metavar? #'foo_1 #'foo))
  (check-true
   (syntax-parse #'foo_1
     [(~var _ (pattern-metavar #'foo)) #t]
     [_ #f]))
  (check-false (matches-metavar? #'bar_1 #'foo))
  (check-false (matches-metavar? #'bar_1 #'bar))


  ;; I'm concerned that I didn't get matches-metavar? right and that
  ;; it somehow gets scopes "wrong." I'll need it to work for
  ;; identifiers that I introduce during expansion (those identifiers
  ;; are based on syntax the user wrote) and I I'll need those
  ;; generated identifiers to match like metavariables. These
  ;; macro-using tests are kinda sorta related to my vague concern;
  ;; TBH I don't know exactly what I'm trying to do with these tests.
  (define-syntax (matches-metavar?-works-with-expansion stx)
    (syntax-parse stx
      [(_ id:id) #'(define id 10)]))
  (matches-metavar?-works-with-expansion env)
  (check-true
   (syntax-parse #'env_1
     [(~var _ (pattern-metavar #'env)) #t]
     [_ #f]))

  (define-syntax (matches-metavar?-doesnt-appear-unhygienic stx)
    #'(define sigma 10))
  (matches-metavar?-doesnt-appear-unhygienic)
  (check-false
   (syntax-parse #'sigma_1
     [(~var _ (pattern-metavar #'sigma)) #t]
     [_ #f]))

  ;; More evidence that I don't know exactly what I'm trying to do
  (define-syntax (matches-metavar?-works-with-expansion2 stx)
    #'(begin
        (define gamma 10)
        (check-true
         (syntax-parse #'gamma_1
           [(~var _ (pattern-metavar #'gamma)) #t]
           [_ #f]))))
  (matches-metavar?-works-with-expansion2))

(define (matches-metavar? pattern id)
  (define without-suffix
    (match (symbol->string (syntax-e pattern))
      [(regexp #px"([^_]+)(_.+)?" (list _ contents suffix))
       (define symbol-without-suffix (string->symbol contents))
       (datum->syntax pattern symbol-without-suffix pattern)]))
  (and (identifier-binding id)
       (bound-identifier=? without-suffix id)))
(define-syntax-class (pattern-metavar id)
  (pattern x:id
           #:when (matches-metavar? #'x id)))

(define-syntax-class e-class
  (pattern (~var _ (pattern-metavar #'e)))
  (pattern :var-class)
  (pattern :v-class)
  (pattern :app-class))
(define-syntax-class app-class
  (pattern (:e-class :e-class)))
(define-syntax-class var-class
  (pattern (~var _ (pattern-metavar #'var)))
  (pattern :x-class)
  (pattern :y-class)
  (pattern :z-class))
(define-syntax-class x-class
  (pattern (~literal x)))
(define-syntax-class y-class
  (pattern (~literal y)))
(define-syntax-class z-class
  (pattern (~literal z)))
(define-syntax-class v-class
  (pattern (~var _ (pattern-metavar #'v)))
  (pattern :lam-class))
(define-syntax-class lam-class
  (pattern ((~literal lam) :var-class :e-class)))

(module+ test
  (check-true (syntax-parse #'(lam x (lam y x))
                [((~literal lam) :var-class e:e-class) #t]
                [_ #f])))
