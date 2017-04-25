#lang racket
(require racket/match syntax/parse)
(provide pattern-metavar)
(define (matches-metavar? pattern id)
  (define without-suffix
    (match (symbol->string (syntax-e pattern))
      [(regexp #px"([^_]*)(_.+)?" (list _ contents suffix))
       (define symbol-without-suffix (string->symbol contents))
       (datum->syntax pattern symbol-without-suffix pattern)]))
  (free-identifier=? without-suffix id))

(define-syntax-class (pattern-metavar id)
  #:description (format "metavar ~a" (syntax-e id))
  (pattern x:id
           #:when (matches-metavar? #'x id)))
