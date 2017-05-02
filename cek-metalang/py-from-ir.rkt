#lang racket
(require "ir.rkt")
(provide class-def->py)

;; TODO Python identifiers are less liberal than Racket identifiers,
;; so we need to make sure we don't emit ill-formed Python. Either
;; limit our use of Racket identifiers to only Python-compatible ones
;; or map them safe-for-Python names here. Some Racket identifier
;; parts that will not work in Python are
;; - dash (e.g. e-comb0)
;; - star (e.g. c*)

;; TODO since Python doesn't have let, we'll also need to ensure any
;; conflicting identifiers in a method name get renamed appropriately

(define (class-name->py name)
  ;; Since metavariables overlap with class names, we need a way to
  ;; disambiguate the two in Python. I'm opting for mangling class
  ;; names---it isn't the most principled approach but it's simple
  ;; enough to work for my examples.
  (format "cl_~a" name))

;; class-def->py : ir:class-def -> string
;; where cdef's super-name is not #f
(define (class-def->py cdef)
  (match cdef
    [(ir:class-def name super-name fdefs mdef)
     (define header (format "class ~a(~a):"
                            (class-name->py name)
                            (if (equal? 'top super-name)
                                "object"
                                (class-name->py super-name))))
     (define constructor (field-defs->constructor-py name fdefs #:indent "  "))
     (define method (method-def->py mdef #:indent "  "))
     (~a header constructor method #:separator "\n")]))

(module+ test
  (define app-def (ir:class-def
                   'app 'e
                   (list (ir:field-def 'app_fn 'e)
                         (ir:field-def 'app_arg 'e))
                   (ir:method-def '(self env k)
                                  (ir:return '(self env k)))))
  (check-equal? (class-def->py app-def)
                (string-join
                 (list
                  "class cl_app(cl_e):"
                  "  def __init__(self, app_fn, app_arg):"
                  "    self.app_fn = app_fn"
                  "    self.app_arg = app_arg"
                  "  def interpret(self, env, k):"
                  "    return self, env, k")
                 "\n")))

;; field-defs->constructor-py : name (U #f (listof field-def)) -> string 
(define (field-defs->constructor-py class-name fdefs #:indent [prefix ""])
  ;; TODO Fix this inconcistency: we explicitly add the self argument
  ;; to the constructor, yet for method-def's we rely on the self
  ;; argument being in the IR
  (match fdefs
    [#f
     (define header (format "~adef __init__(self):" prefix))
     (define body (format "~aself.literal = ~s" prefix (~a class-name)))
     (format "~a\n  ~a" header body)]
    [(list) ;; explicit empty case for similar reasons as ir:return
            ;; but also since we need to emit "pass"
     (define header (format "~adef __init__(self):" prefix))
     (define body (format "~apass" prefix))
     (format "~a\n  ~a" header body)]
    [_
     (define (fdef->arg d) (~a (ir:field-def-fieldname d)))
     (define header
       (format "~adef __init__(self, ~a):" prefix (string-join (map fdef->arg fdefs) ", ")))

     (define (fdef->assign d)
       (define fieldname (ir:field-def-fieldname d))
       (format "~a  self.~a = ~a" prefix fieldname fieldname))
     (define bodies (map fdef->assign fdefs))
     (string-join (cons header bodies) "\n")]))

(module+ test
  (check-equal? (field-defs->constructor-py 'mt #f)
                (string-join
                 (list
                  "def __init__(self):"
                  "  self.literal = \"mt\"")
                 "\n"))
  (check-equal? (field-defs->constructor-py 'unit '())
                (string-join
                 (list
                  "def __init__(self):"
                  "  pass")
                 "\n"))
  (define fdefs (list (ir:field-def 'app_fn 'top)
                      (ir:field-def 'app_arg 'top)))
  (check-equal? (field-defs->constructor-py 'app fdefs)
                (string-join
                 (list
                  "def __init__(self, app_fn, app_arg):"
                  "  self.app_fn = app_fn"
                  "  self.app_arg = app_arg")
                 "\n")))


;; method-def->py : (U 'super ir:method-def) -> string
(define (method-def->py mdef #:indent [prefix ""])
  (match mdef
    ['super
     (format "~a# method inherited from super class" prefix)]
    [(ir:method-def args body)
     (define header (format "~adef interpret(~a):" prefix (apply ~a #:separator ", " args)))
     (string-join (list header (ir->py body #:indent (string-append prefix "  "))) "\n")]))

(module+ test
  (check-equal? (method-def->py 'super) ;; low-value test...
                "# method inherited from super class")

  ;; TODO currently, upstream code puts the self argument explicitly
  ;; in the IR. I'm not sure if that's what we necessarily want; these
  ;; tests assume that it is what we want.
  (check-equal? (method-def->py (ir:method-def '() (ir:return '())))
                (string-join
                 (list
                  "def interpret():"
                  "  return")
                 "\n"))

  (define method-3args
    (ir:method-def '(self env k)
                   (ir:return '(self env k))))
  (check-equal? (method-def->py method-3args)
                (string-join
                 (list
                  "def interpret(self, env, k):"
                  "  return self, env, k")
                 "\n")))


;; ir->py : ir -> string
(define (ir->py ir #:indent [prefix ""])
  (match ir
    [(ir:check-instance n class-name rest)
     (define guard (format "~aif not(isinstance(~a, ~a)):"
                           prefix
                           n
                           (class-name->py class-name)))
     (define failure-message (format "Expected ~a to be an ~a" n class-name))
     (define failure (format "~araise Exception(~s)" prefix failure-message))

     (format "~a\n  ~a\n~a"
             guard
             failure
             (ir->py rest #:indent prefix))]
    [(ir:let (list (list lhss rhss) ...) rest)
     (define (binding-pair->py name simple-ir)
       (format "~a~a = ~a" prefix name (simple-ir->py simple-ir)))
     (define py-assignments (map binding-pair->py lhss rhss))

     (apply ~a #:separator "\n" (append py-assignments (list (ir->py rest #:indent prefix))))]
    [(ir:send receiver args)
     (format "~a~a.interpret(~a)"
             prefix
             receiver
             (apply ~a #:separator ", " args))]
    ;; We have two cases for ir:return because I couldn't figure out
    ;; how produce "return" when results is empty while still putting
    ;; a space after the return keyword when results is not empty. It
    ;; looked using string-join with a #:before-first argument would
    ;; work, but that apparently adds the argument to the result even
    ;; when the list of strings is empty.
    [(ir:return (list))
     (format "~areturn" prefix)]
    [(ir:return results)
     (format "~areturn ~a" prefix (apply ~a results #:separator ", "))]
    [(ir:error message)
     (format "~araise Exception(~s)" prefix message)]))

(module+ test
  (check-equal? (ir->py (ir:return '()))
                "return")
  (check-equal? (ir->py (ir:check-instance 'e1 'e (ir:return '(e1))))
                (string-join
                 (list
                  "if not(isinstance(e1, cl_e)):"
                  "  raise Exception(\"Expected e1 to be an e\")"
                  "return e1")
                 "\n"))
  (check-equal? (ir->py (ir:check-instance 'e1 'e (ir:return '(e1))) #:indent "  ")
                (string-join
                 (list
                  "  if not(isinstance(e1, cl_e)):"
                  "    raise Exception(\"Expected e1 to be an e\")"
                  "  return e1")
                 "\n"))
  (check-equal? (ir->py (ir:let '() (ir:return '(e1))))
                "return e1")
  (check-equal? (ir->py (ir:let '((e2 e1)
                                  (k2 k1))
                                (ir:return '(e2 k2))))
                (string-join
                 (list
                  "e2 = e1"
                  "k2 = k1"
                  "return e2, k2")
                 "\n"))
  (check-equal? (ir->py (ir:let '((e2 e1))
                                (ir:let '((e3 e2))
                                        (ir:return '(e3 e2 e1))))
                        #:indent "  ")
                (string-join
                 (list
                  "  e2 = e1"
                  "  e3 = e2"
                  "  return e3, e2, e1")
                 "\n"))

  (check-equal? (ir->py (ir:send 'x '()))
                "x.interpret()")
  (check-equal? (ir->py (ir:send 'k '(v env)))
                "k.interpret(v, env)")
  (check-equal? (ir->py (ir:return '(c e k)))
                "return c, e, k")
  (check-equal? (ir->py (ir:error "Expected c but got e"))
                "raise Exception(\"Expected c but got e\")"))
  

;; simple-ir->py : simple-ir -> string
(define (simple-ir->py simple-ir)
  (match simple-ir
    [(ir:make name #f)
     (format "~a()" (class-name->py name))]
    [(ir:make name args)
     (format "~a(~a)"
             (class-name->py name)
             (apply ~a #:separator ", " args))]
    [(ir:project _ field-name arg)
     (format "~a.~a" arg field-name)]
    [(ir:call-builtin name args)
     (format "~a(~a)"
             name
             (apply ~a #:separator ", " args))]
    [name (symbol->string name)]))

(module+ test
  (require rackunit)

  (check-equal? (simple-ir->py (ir:make 'mt #f))
                "cl_mt()")
  (check-equal? (simple-ir->py (ir:make 'nil '()))
                "cl_nil()")
  (check-equal? (simple-ir->py (ir:make 'app '(e_1 e_2)))
                "cl_app(e_1, e_2)")

  (check-equal? (simple-ir->py (ir:project 'mumble 'appfirst 'e))
                "e.appfirst")

  (check-equal? (simple-ir->py (ir:call-builtin 'emptyenv '()))
                "emptyenv()")
  (check-equal? (simple-ir->py (ir:call-builtin 'extend '(e1 x v)))
                "extend(e1, x, v)"))
