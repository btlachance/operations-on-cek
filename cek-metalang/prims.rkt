#lang racket
(require "rep.rkt")
(provide variable number string)

(module variable-prim racket
  (require "ir.rkt" "compile.rkt" racket/syntax)
  (provide (prefix-out variable- (all-defined-out)))
  (define ty 'variable)
  (define ((mk/tc-fun result-fun) var-name)
    (result-fun ty))
  (define (compile-pat var-name source rest)
    (check-instance
     source ty
     rest))
  (define (compile-temp var-name dest rest)
    (define tmp (format-symbol "~a_varname" dest))
    ;; Here, we always call the variable constructor whenever one
    ;; occurs in a template. If instead we wanted the variable pattern
    ;; to bind the existing variable (the one the pattern matched
    ;; against) at the IR-level, we would need compile-temp to
    ;; distinguish between two cases
    ;; - when it should use an existing variable bound by a pattern
    ;; - when it should construct a new variable
    ;; I don't know how to accomplish that yet (I think we would need
    ;; to maintain an environment for the IR) so for now we'll just
    ;; always construct a new variable. So long as they don't have
    ;; object identity that should be fine.
    (ir:let (list (list dest (ir:call-builtin 'mkvariable (list (symbol->string var-name)))))
            rest)))
(require 'variable-prim)
(define variable
  (prim-data
   (variable-mk/tc-fun tc-template-result)
   (variable-mk/tc-fun (lambda (ty) (tc-pattern-result ty '())))
   variable-compile-temp
   variable-compile-pat
   variable-ty
   variable-ty))

(module number-prim racket
  (require "ir.rkt" "compile.rkt" racket/syntax)
  (provide (prefix-out number- (all-defined-out)))
  (define ty 'number)
  (define ((mk/tc-fun result-fun) var-name)
    (result-fun ty))
  (define (compile-pat num source rest)
    (define tmp (format-symbol "~a_cmp" source))
    (define failure-msg (format "Expected ~a to equal ~a but it wasn't" source num))
    ;; There is an isinstance check in the equality test for number
    ;; prims; for consistency's sake we'll also check its type via IR.
    (check-instance
     source ty
     (ir:let
      (list (list tmp (ir:call-builtin 'mknum (list num))))
      (ir:if (ir:is-equal source tmp)
             rest
             (ir:match-failure failure-msg)))))
  (define (compile-temp num dest rest)
    (ir:let (list (list dest (ir:call-builtin 'mknum (list num))))
            rest)))
(require 'number-prim)
(define number
  (prim-data
   (number-mk/tc-fun tc-template-result)
   (number-mk/tc-fun (lambda (ty) (tc-pattern-result ty '())))
   number-compile-temp
   number-compile-pat
   number-ty
   number-ty))

(module string-prim racket
  (require "ir.rkt" "compile.rkt" racket/syntax)
  (provide (prefix-out string- (all-defined-out)))
  (define ty 'string)
  (define ((mk/tc-fun result-fun) var-name)
    (result-fun ty))
  (define (compile-pat str source rest)
    (define tmp (format-symbol "~a_cmp" source))
    (define failure-msg (format "Expected ~a to equal ~a but it wasn't" source str))
    (check-instance
     source ty
     (ir:let
      (list (list tmp (ir:call-builtin 'mkstr (list str))))
      (ir:if (ir:is-equal source tmp)
             rest
             (ir:match-failure failure-msg)))))
  (define (compile-temp str dest rest)
    (ir:let (list (list dest (ir:call-builtin 'mkstr (list str))))
            rest)))
(require 'string-prim)
(define string
  (prim-data
   (string-mk/tc-fun tc-template-result)
   (string-mk/tc-fun (lambda (ty) (tc-pattern-result ty '())))
   string-compile-temp
   string-compile-pat
   string-ty
   string-ty))
