#lang racket
(require
 (for-syntax racket/base)
 syntax/id-set
 syntax/parse)
(provide make-keywords-delta-introducer)
(define-syntax (keywords stx)
  (syntax-case stx ()
    [(_ id ...)
     (begin
       #`(begin
           (define-syntax (id stx)
             (raise-syntax-error 'cek-metalang "out of context" stx)) ...
             (provide id) ...
             (define keywords-set
               (immutable-free-id-set (list #'id ...)))
             (provide keywords-set)))]))
(keywords ::= -->
          natural
          default-env lookup extend)
(define (make-keywords-delta-introducer ext-stx)
  (make-syntax-delta-introducer ext-stx #'::))
