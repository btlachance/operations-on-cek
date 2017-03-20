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

(require 'lang-info)
(define-syntax (foo stx)
  (println (syntax-local-value #'var))
  #'(void))
(foo)
