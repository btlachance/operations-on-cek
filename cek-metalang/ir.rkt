#lang racket/base
(provide (all-defined-out))

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
;; - (ir:call-builtin name (listof name))
(struct ir:make (class-name args) #:transparent)
(struct ir:project (class-name field-name arg) #:transparent)
(struct ir:call-builtin (name args) #:transparent)

;; an ir:class-def is (ir:class-def name super-name (U #f (listof field-def)) method-def)
(struct ir:class-def (name super-name fields method) #:transparent)

;; an ir:field-def is (ir:field-def name name)
(struct ir:field-def (fieldname classname) #:transparent)

;; an ir:method-def is (ir:method-def (listof name) IR)
(struct ir:method-def (args body) #:transparent)
