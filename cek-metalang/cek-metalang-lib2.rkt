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
  (define parse-cons-template (form-parse-template cons-form))
  (if (false? (parse-cons-template #'(cons (cons x number_2) number_3)
                                   (list (binding #'number_1 #'number)
                                         (binding #'number_2 #'number)
                                         (binding #'number_3 #'number))))
      (println "cons failed")
      (println "cons success!"))

  (define numbers-form (syntax-local-value #'numbers))
  (define parse-numbers-template (form-parse-template numbers-form))
  (if (false? (parse-numbers-template #'(numbers 1 number_1 3)
                                      (list (binding #'number_1 #'number))))
      (println "numbers failed")
      (println "numbers success!")))
