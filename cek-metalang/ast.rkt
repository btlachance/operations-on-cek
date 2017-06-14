#lang racket
(provide (all-defined-out))

;; a ast is one of
;; - symbol, representing a terminal
;; - (metavar symbol (U #f suffix), where nt is the symbol for some
;;   nonterminal. if the suffix is non-#f then it represents the
;;   suffix in a metavariable, e.g. the 0 in e_0
;; - (metafunction symbol (listof ast) sort)
;;   binding bound to ...? (something that knows how to compile it)
;; - (prim any/c prim-info)
;; - (compound (listof ast) sort), representing a non-atomic form like cons
(struct metavar (nt suffix) #:transparent)
(struct metafunction (name args sort) #:transparent)
(struct prim (payload data) #:transparent)
(struct compound (asts sort) #:transparent)

;; an ast* is one of
;; - (pattern ast type)
;; - (where ast ast)
;; - (template ast type)
(struct pat* (ast expected-ty))
(struct where* (temp-ast pat-ast))
(struct temp* (ast expected-ty))
