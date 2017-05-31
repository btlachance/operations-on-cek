#lang racket
(provide
 (struct-out metavar)
 (struct-out metafunction)
 (struct-out prim)
 (struct-out compound))

;; a ast is one of
;; - symbol, representing a terminal
;; - (metavar symbol (U #f suffix), where nt is the symbol for some
;;   nonterminal. if the suffix is non-#f then it represents the
;;   suffix in a metavariable, e.g. the 0 in e_0
;; - (metafunction symbol (listof ast) sort)
;;   binding bound to ...? (something that knows how to compile it)
;; - (prim any/c id) where the id has a transformer binding bound to
;;   a prim-info
;; - (compound (listof ast) sort), representing a non-atomic form like cons
(struct metavar (nt suffix) #:transparent)
(struct metafunction (name args sort) #:transparent)
(struct prim (data id) #:transparent)
(struct compound (asts sort) #:transparent)
