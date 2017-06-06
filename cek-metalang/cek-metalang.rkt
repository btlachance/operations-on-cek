#lang racket
(require
 (for-syntax
  syntax/parse
  racket/syntax
  racket/sequence
  (only-in "cek-metalang-lib2.rkt"
           -production
           -step))
 (only-in "cek-metalang-lib2.rkt"
          compile-cek))

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
     (with-syntax ([term->py (format-id #'name "~a-term->py" #'name)]
                   [print-interp (format-id #'name "print-~a-interp" #'name)]
                   [(productions ...) (attribute grammar.data)]
                   [(steps ...) (attribute s.data)])
       #`(define-values (print-interp term->py)
           (compile-cek
            #'name
            (list productions ...)
            #'c-nonterminal
            #'e-nonterminal
            #'k-nonterminal
            (list steps ...))))]))
