#lang racket
(require rackunit)
(provide (struct-out language)
         (struct-out binding)
         binding-equal?
         check-binds?
         check-not-binds?
         (struct-out pattern-presult)
         (struct-out form))

;; A language is a (language (listof form))
(struct language (forms))

;; A type is an id with a transformer binding. The value for that
;; binding must be a form

;; A binding is a (binding id type) where the first id is the
;; identifier being bound and the second id represents the
;; identifier's type
(struct binding (id type))
(define (binding-equal? b1 b2)
  (and (free-identifier=? (binding-id b1) (binding-id b2))
       (free-identifier=? (binding-type b1) (binding-type b2))))
(define-binary-check (check-binds? actual expected)
  (define result actual)
  (and (pattern-presult? result)
       (memf (lambda (b) (binding-equal? b expected))
             (pattern-presult-bindings actual))))
(define-binary-check (check-not-binds? actual unexpected)
  (define result actual)
  (or (false? result)
      (not (memf (lambda (b) (binding-equal? b unexpected))
                 (pattern-presult-bindings actual)))))

;; A pattern-presult is one of
;; - #f
;; - (pattern-presult type (listof binding)) representing the result
;;   of succesfully parsing a pattern's syntax
(struct pattern-presult (type bindings))

(struct form (type ;; type -- I'm not certain we'll want this but it
              ;; seems like we will for things like lookup and
              ;; extend metafunctions
              parse-pattern ;; syntax -> pattern-presult
              parse-template
              compile-pattern
              compile-template
              is-default?))
