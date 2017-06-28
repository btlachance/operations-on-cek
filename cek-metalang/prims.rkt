#lang racket
(require "rep.rkt")
(provide variable)

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
