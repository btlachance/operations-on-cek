#lang racket/base
(provide (all-defined-out))

;; a name is a symbol

;; a IR is one of
;; - (ir:if test-IR IR IR)
;; - (ir:if-match-fails (IR IR))
;; - (ir:let (listof (list name simple-IR)) IR)
;; - (ir:send name (listof name))
;; - (ir:return (listof name))
;; - (ir:error string)
;; - (ir:match-failure string)
;; - (ir:unless-failure)
(struct ir:if (test then else) #:transparent)
(struct ir:if-match-fails (cmd then) #:transparent)
(struct ir:let (bindings rest) #:transparent)
(struct ir:send (receiver args) #:transparent)
(struct ir:return (results)  #:transparent)
(struct ir:error (message) #:transparent)
(struct ir:match-failure (message) #:transparent)
(struct ir:unless-failure () #:transparent)

;; TODO Figure out if there's a better way to compile #:unless.
;; Currently the IR for if-match-fails and unless-failure are meant to
;; be used in pairs (see how if-match-fails is compiled in ir->py),
;; and that pairing feels really odd.

;; a test-IR is one of
;; - (ir:is-instance name name)
;; - (ir:is-equal name name)
(struct ir:is-instance (arg class-name) #:transparent)
(struct ir:is-equal (arg1 arg2) #:transparent)

;; a simple-IR is one of
;; - name
;; - (ir:make name (U #f (listof name)))
;; - (ir:project name name name)
;; - (ir:call-builtin name (listof (U name string exact-integer)))
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
(define singleton-class? (compose not ir:class-def-fields))

;; an ir:field-def is (ir:field-def name name)
(struct ir:field-def (fieldname classname) #:transparent)

;; an ir:method-def is one of
;; - (ir:method-def (listof name) (listof IR))
;; - (ir:unimplemented-method string)
(struct ir:method-def (args cases) #:transparent)
(struct ir:unimplemented-method (msg) #:transparent)

;; TODO There is a invariant lurking somehwere in the IR datatype that
;; all IR will (when compiled to Python) cause either an exception to
;; be raised or a value to be returned---i.e. there isn't a "default"
;; return. Is there a name for this sort of guarantee? (It's odd to
;; phrase it in terms of the resulting Python, but I don't know how to
;; phrase it in terms of the IR.)
