#lang racket
(require
 (for-syntax
  syntax/parse
  racket/syntax
  racket/sequence
  "cek-metalang-lib.rkt"))

(require "keywords.rkt")
(provide (all-from-out "keywords.rkt"))
(provide define-cek)

;; For each right r, we need to generate a collection of the
;; steps that happen when e = r. For each step, its lhs' e must
;; belong to a single r---this allows us to later put the steps
;; for an entire r into a single class' interpret method (this
;; is more restrictive than just requiring it to be
;; deterministic).

;; e.g. this step (the only one for (add1 e))
;;     [((add1 e) env k) --> (e env (:: inc k))]
;; will generate a function definition like
;;     (define (add1-step add1-e env k)
;;       ...
;;       (values e env (cont-push inc k)))
;; but these two steps (the only two for n)
;;     [(0 env (:: inc k)) --> (1 env k)]
;;     [(n env (:: inc k)) --> (,(add1 (term n)) env k)
;;      #:when ,(not (zero? (term n)))]
;; will generate a function definition like
;;     (define (n-step e env k)
;;       ... ;; something about matching on k
;;       (cond
;;        [(zero? e)
;;         (values 1 env k*)]
;;        [(call-prim (not (call-prim zero? e)))
;;         (values (call-prim (add1 e) env k*))])

;; TODO start chipping away at this
(define-syntax (define-cek stx)
  (syntax-parse stx
    [(define-cek name:id
       #:grammar
       grammar:production ...
       #:control-string c-nonterminal:id
       #:environment e-nonterminal:id
       #:continuation k-nonterminal:id
       #:step
       s:step ...)
     (compile-cek
      #'name
      (attribute grammar.data)
      #'c-nonterminal
      #'e-nonterminal
      #'k-nonterminal
      (attribute s.data))]))