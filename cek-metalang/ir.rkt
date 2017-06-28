#lang racket/base
(provide (all-defined-out))

;; a name is a symbol

;; a IR is one of
;; - (ir:check-instance name name IR)
;; - (ir:let (listof (list name simple-IR)) IR)
;; - (ir:send name (listof name))
;; - (ir:return (listof name))
;; - (ir:error string)
(struct ir:check-instance (arg class-name rest) #:transparent)
(struct ir:let (bindings rest) #:transparent)
(struct ir:send (receiver args) #:transparent)
(struct ir:return (results)  #:transparent)
(struct ir:error (message) #:transparent)

;; a simple-IR is one of
;; - name
;; - (ir:make name (U #f (listof name)))
;; - (ir:project name name name)
;; - (ir:call-builtin name (listof (U name string)))
(struct ir:make (class-name args) #:transparent)
(struct ir:project (class-name field-name arg) #:transparent)
(struct ir:call-builtin (name args) #:transparent)

;; a super-class is one of
;; - 'top
;; - name, where name != 'top


;; TODO Right now it's tricky for the class-def IR to distinguish
;; between a nonterminal (e.g. k) and a form with no fields (e.g. a
;; made-up one like (foo) where foo is a terminal). Currently, fields
;; would be '() for k and for (foo). (It's not #f for k because we use
;; #f to signal that it's a non-compound terminal class e.g. mt.) It
;; would be nice to distinuish between k and (foo) so that we can
;; raise a runtime error in the constructor for k

;; an ir:class-def is (ir:class-def name super-class (U #f (listof ir:field-def)) (U 'super ir:method-def))
(struct ir:class-def (name super-name fields method) #:transparent)

;; an ir:field-def is (ir:field-def name name)
(struct ir:field-def (fieldname classname) #:transparent)

;; an ir:method-def is one of
;; - (ir:method-def (listof name) (listof IR))
;; - (ir:unimplemented-method string)
(struct ir:method-def (args cases) #:transparent)
(struct ir:unimplemented-method (msg) #:transparent)
