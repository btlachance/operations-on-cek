#lang racket
(require
 racket/syntax
 syntax/parse
 (for-template racket/base))

(provide production step)
(module+ test
  (require rackunit))

(define (out-of-context stx)
  (raise-syntax-error
   'cek-metalang
   "used out of context"
   stx))

(module keywords racket/base
  (require (for-syntax racket/base))
  (define-syntax (keywords stx)
    (syntax-case stx ()
      [(_ id ...)
       #'(begin
           (define-syntax (id stx)
             (raise-syntax-error 'cek-metalang "out of context")) ...
             (provide id) ...)]))
  (keywords ::= -->
            natural
            default-env lookup extend
            default-mt ::))
(require (for-template 'keywords))

(define-syntax-class production
  #:attributes (name (form 1))
  #:literals (::=)
  (pattern (name:id ::= form ...+)))

(define-syntax-class step
    #:attributes (lhs rhs)
    #:literals (-->)
    (pattern [(~and lhs (e-l env-l k-l))
              -->
              (~and rhs (e-r env-r k-r))
              #:implemented-by form]))
