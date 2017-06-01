#lang racket
(require "ast.rkt")
(provide
 (all-from-out "ast.rkt")
 (struct-out nt)
 (struct-out binding)
 lookup-ty
 (struct-out tc-template-result)
 (struct-out tc-pattern-result))

;; a sort is one of
;; - symbol, representing a terminal
;; - (listof (U nonterminal symbol))

;; a nonterminal is a (nt symbol)
(struct nt (symbol) #:transparent)

;; a suffix is one of
;; - natural
;; - symbol

;; a binding is a (binding metavar type)
(struct binding (metavar type) #:transparent)

;; lookup-ty : metavar (listof binding) -> (U type #f)
(define (lookup-ty metavar bindings)
  (define (find-metavar bs)
    (match bs
      [(list b bs* ...)
       (if (equal? (binding-metavar b) metavar)
           (binding-type b)
           (find-metavar bs*))]
      [_ #f]))
  (find-metavar bindings))

;; a tc-template-result is one of
;; - #f
;; - (tc-template-result type)
(struct tc-template-result (type) #:transparent)

(struct tc-pattern-result (type bindings) #:transparent)
