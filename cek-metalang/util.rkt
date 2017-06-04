#lang racket
(require racket/syntax "rep.rkt")
(provide metavar->symbol)
;; The IR currently expects symbols for variable names. Later, though,
;; it might be worth making it use metavar's
(define (metavar->symbol mv)
  (match mv
    [(metavar nt #f) nt]
    [(metavar nt suffix)
     (format-symbol "~a_~a" nt suffix)]))
