#lang racket
(module prims racket
  (require (for-syntax "prims-impl.rkt"))
  (provide variable number numbers cons)
  (define-syntax variable (mk-variable #'variable))
  (define-syntax number (mk-number #'number))
  (define-syntax numbers (mk-numbers #'numbers #'number))
  (define-syntax cons (mk-cons #'cons (list #'variable #'number #'numbers))))

(require 'prims (for-syntax "rep.rkt" racket/bool))
(begin-for-syntax
  (define cons-form (syntax-local-value #'cons))
  (define parse-cons-pattern (form-parse-pattern cons-form))
  (if (false? (parse-cons-pattern #'(cons (cons number_1 number_2) number_3)))
      (println "failed")
      (println "success!")))
