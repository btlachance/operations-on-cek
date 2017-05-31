#lang racket
(require
 (for-syntax
  syntax/parse
  racket/syntax
  racket/sequence
  "cek-metalang-lib2.rkt"))

(provide define-cek)

(define-syntax (define-cek stx)
  (syntax-parse stx
    [(define-cek name:id
       #:grammar
       grammar:-production ...
       #:control-string c-nonterminal:id
       #:environment e-nonterminal:id
       #:continuation k-nonterminal:id
       #:step
       s:-step ...)
     (compile-cek
      #'name
      (attribute grammar.data)
      #'c-nonterminal
      #'e-nonterminal
      #'k-nonterminal
      (attribute s.data))]))
