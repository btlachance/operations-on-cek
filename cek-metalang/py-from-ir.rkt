#lang racket
(require "ir.rkt")
(provide class-def->py ir->py)

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
(define (class-def->py cdef #:debug? [debug? #t])
  (match cdef
    [(ir:class-def name super-name fdefs mdef)
     (define header (format "class ~a(~a):"
                            (class-name->py name)
                            (if (equal? 'top super-name)
                                "object"
                                (class-name->py super-name))))
     (define attrs (field-defs->attrs fdefs #:indent "  "))
     (define constructor (field-defs->constructor-py name fdefs #:indent "  "))
     (define method (method-def->py name mdef #:indent "  " #:super super-name))
     (string-append
      (~a header
          attrs
          constructor
          method
          #:separator "\n")
      (if debug?
          (~a
           ""
           (make-pprint-method name fdefs)
           #:separator "\n")
          "")
      (if (not fdefs) ;; XXX The ignore terminal means terminals can
                      ;; be control strings, and so they need to be
                      ;; singletons
          (~a
           ""
           (format "val_~a_sing = ~a()" name (class-name->py name))
           #:separator "\n")
          ""))]))

(module+ test
  (define app-def (ir:class-def
                   'app 'e
                   (list (ir:field-def 'app_fn 'e)
                         (ir:field-def 'app_arg 'e))
                   (ir:method-def '(self env k)
                                  (list (ir:return '(self env k))))))
  #;(check-equal? (class-def->py app-def #:debug? #f)
                (string-join
                 (list
                  "class cl_app(cl_e):"
                  "  def __init__(self, app_fn, app_arg):"
                  "    self.app_fn = app_fn"
                  "    self.app_arg = app_arg"
                  "  def interpret(self, env, k):"
                  "    try:"
                  "      return self, env, k"
                  "    except CEKMatchFailure as matchf:"
                  "      pass"
                  "    raise CEKError(\"No cases matched for method in class app\")")
                 "\n")))

(define (make-pprint-method class-name field-defs)
  (match field-defs
    [#f
     (string-join
      (list
       "  def pprint(self, indent):"
       (format "    return ' ' * indent + '~a'" class-name))
      "\n")]
    [(list)
     (string-join
      (list
       "  def pprint(self, indent):"
       (format "    return ' ' * indent + '(~a)'" class-name))
      "\n")]
    [(list defs ...)
     (define (fdef->print-field d)
       (format "self.~a.pprint(0)" (ir:field-def-fieldname d)))
     (string-join
      (list
       "  def pprint(self, indent):"
       (format "    return ' ' * indent + '(~a ~a)' % (~a)"
               class-name
               (apply ~a (for/list ([d defs]) "%s") #:separator " ")
               (apply ~a (for/list ([d defs]) (fdef->print-field d)) #:separator ", ")))
      "\n")]))
(module+ test
  (check-equal? (make-pprint-method 'mt #f)
                (string-join
                 (list
                  "  def pprint(self, indent):"
                  "    return ' ' * indent + 'mt'")
                 "\n"))
  (define lam-pprint-fdefs (list (ir:field-def 'x 'top)
                                 (ir:field-def 'e 'top)))
  (check-equal? (make-pprint-method 'lam lam-pprint-fdefs)
                (string-join
                 (list
                  "  def pprint(self, indent):"
                  "    return ' ' * indent + '(lam %s %s)' % (self.x.pprint(0), self.e.pprint(0))")
                 "\n")))

;; field-defs->constructor-py : name (U #f (listof field-def)) -> string 
(define (field-defs->constructor-py class-name fdefs #:indent [prefix ""])
  ;; TODO Fix this inconcistency: we explicitly add the self argument
  ;; to the constructor, yet for method-def's we rely on the self
  ;; argument being in the IR
  (match fdefs
    [#f
     (define header (format "~adef __init__(self):" prefix))
     (define body (format "~apass" prefix))
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
                  "  pass")
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

(define (field-defs->attrs fdefs #:indent [prefix ""])
  (define attrs-rhs
    (match fdefs
      [(or #f (list)) ""]
      [(list (ir:field-def names _) ...)
       (define (format-attr-name s) (format "'~a'" s))
       (apply ~a #:separator ", " (map format-attr-name names))]))
  (format "~a_attrs_ = [~a]" prefix attrs-rhs))

;; method-def->py : name (U 'super ir:method-def) -> string
(define (method-def->py class-name mdef #:indent [prefix ""] #:super [super-name 'top])
  (match mdef
    ['super
     (format "~a# method inherited from super class" prefix)]
    [(ir:unimplemented-method msg)
     ;; We only know the number of arguments when we emit code for
     ;; implemented methods, and we can't leave the unimplemented
     ;; ones at zero arguments: we don't want Python errors that say
     ;; e.g.  "interpret() takes no arguments (3 given)"
     (define header (format "~adef interpret(*args):" prefix))
     (define error-ir (ir:error msg))
     (string-join (list header (ir->py error-ir #:indent (string-append prefix "  "))) "\n")]
    [(ir:method-def args cases)
     (define header (format "~adef interpret(~a):" prefix (apply ~a #:separator ", " args)))
     (define (case->py ir)
       (ir->py/handle-match-failure ir #:indent (string-append prefix "  ")))
     (define fallthrough-error
       (if (equal? super-name 'top)
           (format "~araise CEKError(~s)"
                   (string-append prefix "  ")
                   (format "No cases matched for method in class ~a" class-name))
           (format "~areturn ~a.interpret(~a)"
                   (string-append prefix "  ")
                   (class-name->py super-name)
                   (apply ~a #:separator ", " args))))
     (~a
      header
      (string-join (map case->py cases) "\n")
      fallthrough-error
      #:separator "\n")]))

(module+ test
  (check-equal? (method-def->py 'blah 'super) ;; low-value test...
                "# method inherited from super class")

  ;; TODO currently, upstream code puts the self argument explicitly
  ;; in the IR. I'm not sure if that's what we necessarily want; these
  ;; tests assume that it is what we want.
  #;(check-equal? (method-def->py 'swimmer (ir:method-def '() (list (ir:return '()))))
                (string-join
                 (list
                  "def interpret():"
                  "  try:"
                  "    return"
                  "  except CEKMatchFailure as matchf:"
                  "    pass"
                  "  raise CEKError(\"No cases matched for method in class swimmer\")")
                 "\n"))

  (define method-3args
    (ir:method-def '(self env k)
                   (list (ir:return '(self env k)))))
  (check-equal? (method-def->py 'infswimmer method-3args)
                (string-join
                 (list
                  "def interpret(self, env, k):"
                  "  try:"
                  "    return self, env, k"
                  "  except CEKMatchFailure as matchf:"
                  "    pass"
                  "  raise CEKError(\"No cases matched for method in class infswimmer\")")
                 "\n")))

;; ir->py/handle-match-failure : ir -> string
(define (ir->py/handle-match-failure ir #:indent [prefix ""])
  (format
   "~atry:\n~a\n~aexcept r.CEKMatchFailure as matchf:\n~a  pass"
   prefix
   (ir->py ir #:indent (string-append prefix "  "))
   prefix
   prefix))

(module+ test
  (check-equal? (ir->py/handle-match-failure (ir:return '(self env k)) #:indent "  ")
                (string-join
                 (list
                  "  try:"
                  "    return self, env, k"
                  "  except CEKMatchFailure as matchf:"
                  "    pass")
                 "\n")))

;; ir->py : ir -> string
(define (ir->py ir #:indent [prefix ""])
  (match ir
    [(ir:if test then else)
     (~a (format "~aif ~a:" prefix (test-ir->py test))
         (ir->py then #:indent (string-append prefix "  "))
         (format "~aelse:" prefix)
         (ir->py else #:indent (string-append prefix "  "))
         #:separator "\n")]
    [(ir:if-match-fails cmd then)
     (~a (format "~atry:" prefix)
         (ir->py cmd #:indent (string-append prefix "  "))
         (format "~aexcept r.CEKMatchFailure as matchf:" prefix)
         (ir->py then #:indent (string-append prefix "  "))
         (format "~aexcept r.CEKUnlessFailure:" prefix)
         (format "~a  pass" prefix)
         #:separator "\n")]
    [(ir:let (list (list lhss rhss) ...) rest)
     (define (binding-pair->py name simple-ir)
       (format "~a~a = ~a" prefix name (simple-ir->py simple-ir)))
     (define py-assignments (map binding-pair->py lhss rhss))

     (apply ~a #:separator "\n" (append py-assignments (list (ir->py rest #:indent prefix))))]
    [(ir:send receiver args)
     (format "~areturn ~a.interpret(~a)"
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
     (format "~araise r.CEKError(~s)" prefix message)]
    [(ir:match-failure message)
     (format "~araise r.CEKMatchFailure(~s)" prefix message)]
    [(ir:unless-failure)
     (format "~araise r.CEKUnlessFailure()" prefix)]))

(module+ test
  (check-equal? (ir->py (ir:if (ir:is-instance 'tofu 'food) (ir:error "then") (ir:error "else")))
                (string-join
                 (list
                  "if isinstance(tofu, cl_food):"
                  "  raise CEKError(\"then\")"
                  "else:"
                  "  raise CEKError(\"else\")")
                 "\n"))
  (check-equal? (ir->py (ir:if-match-fails (ir:match-failure "failed!") (ir:error "bailed anyway")))
                (string-join
                 (list
                  "try:"
                  "  raise CEKMatchFailure(\"failed!\")"
                  "except CEKMatchFailure as matchf:"
                  "  raise CEKError(\"bailed anyway\")"
                  "except CEKUnlessFailure:"
                  "  pass")
                 "\n"))
  (check-equal? (ir->py (ir:return '()))
                "return")
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
                "return x.interpret()")
  (check-equal? (ir->py (ir:send 'k '(v env)))
                "return k.interpret(v, env)")
  (check-equal? (ir->py (ir:return '(c e k)))
                "return c, e, k")
  (check-equal? (ir->py (ir:error "Expected c but got e"))
                "raise CEKError(\"Expected c but got e\")")
  (check-equal? (ir->py (ir:match-failure "Expected e but got mt"))
                "raise CEKMatchFailure(\"Expected e but got mt\")")
  (check-equal? (ir->py (ir:unless-failure))
                "raise CEKUnlessFailure()"))

(define (test-ir->py test-ir)
  (match test-ir
    [(ir:is-instance arg class-name)
     (format "isinstance(~a, ~a)" arg (class-name->py class-name))]
    [(ir:is-equal arg1 arg2)
     (format "~a.eq(~a)" arg1 arg2)]))

(module+ test
  (check-equal? (test-ir->py (ir:is-instance 'person 'swimmer))
                "isinstance(person, cl_swimmer)")
  (check-equal? (test-ir->py (ir:is-equal 'int1 'int2))
                "int1.eq(int2)"))

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
     (format "r.~a(~a)"
             name
             (apply ~s #:separator ", " args))]
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
