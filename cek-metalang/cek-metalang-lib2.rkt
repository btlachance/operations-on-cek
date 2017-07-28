#lang racket
(require
 racket/hash
 racket/syntax
 syntax/stx
 syntax/parse
 "rep.rkt"
 "util.rkt"
 "parse.rkt"
 "typecheck.rkt"
 "compile.rkt"
 "ir.rkt"
 "py-from-ir.rkt"
 (prefix-in prim: "prims.rkt")
 (for-template racket/base))
(provide -production -step -final -initial compile-cek)

(struct production (name forms) #:prefab)
(define-syntax-class -production
  #:attributes (name (form 1) data)
  #:datum-literals (::=)
  (pattern (name:id ::= form ...+)
           #:attr data (production #'name (attribute form))))

(define-splicing-syntax-class clause
  #:attributes (body)
  (pattern :where-clause)
  (pattern :unless-clause))
(define ((mk/parse-clause parse-temp parse-pat) w)
  (match w
    [(list 'where pattern template)
     (where* (parse-temp template) (parse-pat pattern))]
    [(list 'unless pattern template)
     (unless* (parse-temp template) (parse-pat pattern))]))
(define-splicing-syntax-class where-clause
  #:attributes (body)
  (pattern (~seq #:where pattern template)
           #:attr body (list 'where #'pattern #'template)))
(define-splicing-syntax-class unless-clause
  #:attributes (body)
  (pattern (~seq #:unless pattern template)
           #:attr body (list 'unless #'pattern #'template)))

(struct step (lhs rhs clauses) #:prefab)
(define-syntax-class -step
  #:attributes (lhs rhs (clauses 1) data)
  #:datum-literals (-->)
  (pattern [(~and lhs (e-l env-l k-l))
            -->
            (~and rhs (e-r env-r k-r))
            clauses:clause ...]
           #:attr data (step #'lhs #'rhs (attribute clauses.body))))

(struct final (state result) #:prefab)
(define-syntax-class -final
  #:datum-literals (-->)
  (pattern [(~and state (e env k)) --> result]
           #:attr data (final #'state #'result)))

(struct initial (program state) #:prefab)
(define-syntax-class -initial
  #:datum-literals (-->)
  (pattern [program --> (~and state (e env k))]
           #:attr data (initial #'program #'state)))

(define ((form-in-nonterminals? nonterminals) f)
  (memf (lambda (nt) (equal? (nt-symbol nt) f)) nonterminals))

(struct lang-info (;; (setof nt)
                   nonterminals
                   ;; (setof sort)
                   metafunctions
                   ;; (listof prim-parser)
                   prim-parsers
                   ;; (hash sort symbol)
                   sort->name
                   ;; (hash sort (listof symbol))
                   sort->field-names
                   ;; (hash sort type)
                   sort->type
                   ;; (hash name type)
                   metafunction->type
                   ;; (hash type (U type #f))
                   parent-of))

(define (productions->lang-info productions)
  ;; Looking up the prims in this function was tricky for me: because
  ;; it's required for-syntax by cek-metalang, we have to make sure
  ;; that the prims have for-template bindings.
  (define nonterminals
    ;; HACK Need to detect whether the grammar actually uses the
    ;; variable and actually insert the nonterminal into this list
    ;; like we do for other nonterminals
    (append (map (compose nt production-name) productions)
            (list (nt (prim-data-name prim:variable))
                  (nt (prim-data-name prim:integer)))))
  (define nonterminal-form? (form-in-nonterminals? nonterminals))

  (define-values (sort->name sort->field-names sort->type)
    (for/fold ([sort->name (hash)]
               [sort->field-names (hash)]
               [sort->type (hash)])
              ([production-name (map production-name productions)]
               [forms (map production-forms productions)]
               #:when #t
               [form forms]
               [idx (in-naturals)]
               #:when (not (nonterminal-form? form))
               ;; HACK And we need to make sure that primitives /in
               ;; general/ aren't treated like literals
               #:when (not (and (symbol? form)
                                (equal? form 'variable))))
      (cond
        [(symbol? form)
         (values
          (hash-set sort->name form form)
          (hash-set sort->field-names form #f)
          (hash-set sort->type form production-name))]
        [(list? form)
         (define sort
           (for/list ([subform form])
             (if (nonterminal-form? subform)
                 (nt subform)
                 subform)))
         (define sort-name
           (match sort
             [(list (? symbol? s) _ ...) s]
             [_ (format-symbol "~a_comb~a" production-name idx)]))
         (define field-names
           (for/list ([nt (filter nt? sort)]
                      [idx (in-naturals)])
             (format-symbol "~a~a" (nt-symbol nt) idx)))
         (values
          (hash-set sort->name sort sort-name)
          (hash-set sort->field-names sort field-names)
          (hash-set sort->type sort production-name))]
        [else (raise-arguments-error
               'compile-cek
               "unexpected form"
               "form" form)])))

  (define variable-parse-fun
    (syntax-parser
      [:id
       (prim (syntax-e this-syntax) prim:variable)]
      [_ #f]))
  (define integer-parse-fun
    (syntax-parser
      [:exact-integer
       (prim (syntax-e this-syntax) prim:integer)]
      [_ #f]))
  (lang-info nonterminals
             ;; hard-coded metafunctions for now...
             (list (list 'lookup (nt 'env) (nt 'var))
                   (list 'extend (nt 'env) (nt 'vars) (nt 'vs))
                   (list 'extend1 (nt 'env) (nt 'var) (nt 'v))
                   (list 'extendrest (nt 'env) (nt 'vars) (nt 'var) (nt 'vs))
                   (list 'zeropimpl (nt 'var))
                   (list 'succimpl (nt 'var))
                   (list 'predimpl (nt 'var))
                   (list 'addimpl (nt 'var) (nt 'var))
                   (list 'subimpl (nt 'var) (nt 'var))
                   (list 'multimpl (nt 'var) (nt 'var))
                   (list 'boximpl (nt 'var))
                   (list 'unboximpl (nt 'var))
                   (list 'setboximpl (nt 'var) (nt 'var))
                   (list 'modformsreverse (nt 'modforms))
                   (list 'mkcell (nt 'v))
                   (list 'setcell (nt 'var) (nt 'env) (nt 'v))
                   (list 'vsreverse (nt 'vs))
                   (list 'printimpl (nt 'var))
                   (list 'emptyenv))
             ;; TODO hard-code the environment prim
             (list (parser variable-parse-fun variable-parse-fun)
                   (parser integer-parse-fun integer-parse-fun))
             sort->name sort->field-names sort->type
             (hash 'lookup 'v
                   'extend 'env
                   'extend1 'env
                   'extendrest 'env
                   'zeropimpl 'e
                   'succimpl 'e
                   'predimpl 'e
                   'addimpl 'e
                   'subimpl 'e
                   'multimpl 'e
                   'boximpl 'e
                   'unboximpl 'e
                   'setboximpl 'e
                   'modformsreverse 'modforms
                   'mkcell 'v
                   'setcell 'v
                   'vsreverse 'vs
                   'printimpl 'e
                   'emptyenv 'env)
             (mk/parent-of nonterminals productions)))

;; mk/parent-of : (setof nonterminal) (listof production) -> (hash type (U type 'top))
;; INV:
;; - only the production-name in productions have a corresponding
;;   nonterminal in nonterminals
;; - each nonterminal occurs at most once on the right-hand side of a
;;   production
;; - no cycles in the productions (e.g. (e ::= x) (x ::= y) (y ::= e))
(define (mk/parent-of nonterminals productions)
  (define nonterminal-form? (form-in-nonterminals? nonterminals))
  
  (define (nt->no-parent nt)
    (cons (nt-symbol nt) 'top))
  (for/fold ([parent-of (make-immutable-hash (map nt->no-parent nonterminals))])
            ([p productions]
             #:when #t
             [sub (filter symbol? (production-forms p))]
             #:when (nonterminal-form? sub))
    (define super (production-name p))
    (hash-set parent-of sub super)))
;; mk/subtype? : (hash type (U type #f)) -> (type type -> bool)
(define (mk/subtype? parent-of)
  (define (subtype? t1 t2)
    (cond
      [(equal? t1 t2) #t]
      [else
       (and (not (equal? 'top (hash-ref parent-of t1)))
            (subtype? (hash-ref parent-of t1) t2))]))
  subtype?)
(module+ test
  (require rackunit)
  (define subtype?-t1 (mk/subtype?
                       (mk/parent-of (list (nt 'e) (nt 'x) (nt 'v) (nt 'z))
                                     (list (production 'e (list 'x 'v))
                                           (production 'x (list 'variable))
                                           (production 'v (list '(lambda x e) 'z))
                                           (production 'z (list 'foo))))))
  (check-true (subtype?-t1 'x 'e))
  (check-false (subtype?-t1 'e 'x))
  (check-true (subtype?-t1 'v 'e))
  (check-true (subtype?-t1 'z 'v))
  (check-true (subtype?-t1 'z 'e)))

;; compile-cek : id (listof production) id id id (listof step) initial final
;;                 -> (values (-> (void)) (stx -> string))
(define (compile-cek lang-id productions c-id e-id k-id steps initial final)
  (match-define (lang-info nonterminals metafunctions prim-parsers
                           sort->name sort->field-names sort->type
                           metafunction->type parent-of)
    (productions->lang-info productions))

  ;; ast->name : ast -> name
  ;; Given an ast produce its corresponding class' name. I think it
  ;; should only be called on pattern ast's but I'm not certain yet;
  ;; that rules out calling it on metafunctions, which is a good
  ;; thing: those don't have a class.
  (define (ast->name ast)
    (cond
      [(symbol? ast) (hash-ref sort->name ast)]
      [(compound? ast) (hash-ref sort->name (compound-sort ast))]
      [else (metavar-nt ast)]))
  (define-values (terminals compounds) (partition symbol? (hash-keys sort->type)))

  (match-define (parser parse-temp parse-pat)
    (lang-parser terminals nonterminals compounds metafunctions prim-parsers))
  (define-values (tc-temp tc-pat tc-temps/expecteds tc-pats/expecteds tc-ast*s)
    (lang-typechecker
     sort->type
     metafunction->type
     (mk/subtype? parent-of)))
  (define-values (compile-temp compile-pat compile-clauses)
    (lang-compiler sort->field-names sort->name))

  ;; a method is a (method symbol (listof symbol) (U IR (listof IR)))
  ;; HACK a method only has its cases as a single IR (not a list of
  ;; IR) when it's the immediate result of step->methods. Once the
  ;; method is in method-by-class-name cases is a (listof IR)
  (struct method (class-name arg-names cases) #:transparent)
  (define (step->methods step)
    (match-define (list c0-ast e0-ast k0-ast)
      (stx-map parse-pat (step-lhs step)))
    (define clauses (map (mk/parse-clause parse-temp parse-pat) (step-clauses step)))
    (match-define (list c*-ast e*-ast k*-ast)
      (stx-map parse-temp (step-rhs step)))

    (define cek/tys (map syntax-e (list c-id e-id k-id)))
    (tc-ast*s
     (append
      (map pat* (list c0-ast e0-ast k0-ast) cek/tys)
      clauses
      (map temp* (list c*-ast e*-ast k*-ast) cek/tys)))
    (if (or (metavar? k0-ast)
            (prim? k0-ast))
        (list
         (method
          (ast->name c0-ast)
          (list 'self 'e 'k)
          (foldr
           compile-pat
           (compile-clauses
            clauses
            (foldr
             compile-temp
             (ir:return (list 'c_result 'e_result 'k_result))
             (list c*-ast e*-ast k*-ast)
             (list 'c_result 'e_result 'k_result)))
           (list c0-ast e0-ast k0-ast)
           (list 'self 'e 'k))))
        (list
         (method
          (ast->name c0-ast)
          (list 'self 'e 'k)
          (foldr
           compile-pat
           (ir:send 'k (list 'self 'e))
           (list c0-ast e0-ast (metavar 'k #f))
           (list 'self 'e 'k)))
         (method
          (ast->name k0-ast)
          (list 'self 'c_arg 'e_arg)
          (foldr
           compile-pat
           (compile-clauses
            clauses
            (foldr
             compile-temp
             (ir:return (list 'c_result 'e_result 'k_result))
             (list c*-ast e*-ast k*-ast)
             (list 'c_result 'e_result 'k_result)))
           (list k0-ast c0-ast e0-ast)
           (list 'self 'c_arg 'e_arg))))))

  (define (final->methods f)
    (match-define (list c0-ast e0-ast k0-ast)
      (stx-map parse-pat (final-state f)))
    (define result-ast (parse-temp (final-result f)))
    (define cek/tys (map syntax-e (list c-id e-id k-id)))
    (tc-ast*s
     (append (map pat* (list c0-ast e0-ast k0-ast) cek/tys)
             (list (temp* result-ast (syntax-e c-id)))))
    (if (or (metavar? k0-ast)
            (prim? k0-ast))
        (list
         (method
          (ast->name c0-ast)
          (list 'self 'e 'k)
          (foldr
           compile-pat
           (compile-temp
            result-ast
            'result
            (ir:let (list (list 'dummy (ir:call-builtin 'ret (list 'result))))
                    (ir:error "Failed to halt the program in a final state")))
           (list c0-ast e0-ast k0-ast)
           (list 'self 'e 'k))))
        (list
         (method
          (ast->name c0-ast)
          (list 'self 'e 'k)
          (foldr
           compile-pat
           (ir:send 'k (list 'self 'e))
           (list c0-ast e0-ast (metavar 'k #f))
           (list 'self 'e 'k)))
         (method
          (ast->name k0-ast)
          (list 'self 'c_arg 'e_arg)
          (foldr
           compile-pat
           (compile-temp
            result-ast
            'result
            (ir:let (list (list 'dummy (ir:call-builtin 'ret (list 'result))))
                    (ir:error "Failed to halt the program in a final state")))
           (list k0-ast c0-ast e0-ast)
           (list 'self 'c_arg 'e_arg))))))

  (define (initial->method-def i)
    (define c0-ast (parse-pat (initial-program i)))
    (match-define (list c*-ast e*-ast k*-ast)
      (stx-map parse-temp (initial-state i)))
    (define cek/tys (map syntax-e (list c-id e-id k-id)))
    (tc-ast*s
     (cons (pat* c0-ast (syntax-e c-id))
           (map temp* (list c*-ast e*-ast k*-ast) cek/tys)))
    (ir:method-def
     (list 'p)
     (list
      (compile-pat
       c0-ast
       'p
       (foldr
        compile-temp
        (ir:return (list 'c_init 'e_init 'k_init))
        (list c*-ast e*-ast k*-ast)
        (list 'c_init 'e_init 'k_init))))))

  (define method-by-class-name
    (for/fold ([method-map (hash)])
              ([m (in-sequences
                   (apply append (map step->methods steps))
                   (final->methods final))])
      (match* (m (hash-ref method-map (method-class-name m) #f))
        [((method class-name arg-names0 case)
          (method _ arg-names1 cases))
         (unless (equal? arg-names0 arg-names1)
           (error 'compile-cek
                  "class ~a has two methods with differing argument names: ~a and ~a"
                  arg-names0
                  arg-names1))
         (hash-set method-map
                   class-name
                   (method class-name arg-names0 (append cases (list case))))]
        [((method class-name arg-names case) #f)
         (hash-set method-map class-name (method class-name arg-names (list case)))])))

  ;; check-for-super-method : name (U name 'top) -> (U ir:method-def 'super)
  (define (check-for-super-method class-name super-name)
    (let loop ([defining-class super-name])
      (match defining-class
        ['top
         (ir:unimplemented-method
          (format "class ~a does not implement a method" class-name))]
        [_
         (if (hash-has-key? method-by-class-name defining-class)
             'super
             (loop (hash-ref parent-of defining-class)))])))

  (define-values (other-class-defs nt-class-defs)
    (values
     (for/list ([sort (in-sequences compounds terminals)])
       (define class-name (hash-ref sort->name sort))
       (define parent-class-name (hash-ref sort->type sort))
       (ir:class-def
        class-name
        parent-class-name
        (match (hash-ref sort->field-names sort #f)
          [(list field-names ...)
           (for/list ([name field-names])
             (ir:field-def name class-name))]
          [_ #f])
        (match (hash-ref method-by-class-name class-name #f)
          [(method _ args cases)
           (ir:method-def args cases)]
          [_
           (check-for-super-method class-name parent-class-name)])))
     (for/list ([nt nonterminals])
       (define class-name (nt-symbol nt))
       (define parent-class-name (or (hash-ref parent-of class-name) 'top))
       (ir:class-def
        class-name
        parent-class-name
        '() ;; The only time we have a #f field-defs is when the class
            ;; represents a terminal
        (match (hash-ref method-by-class-name class-name #f)
          [(method _ args cases)
           (ir:method-def args cases)]
          [_
           (check-for-super-method class-name parent-class-name)])))))
  (define (print-interpreter)
    (for ([def (in-sequences nt-class-defs other-class-defs)])
      (pretty-display (class-def->py def)))
    (pretty-display
     (match (initial->method-def initial)
       [(ir:method-def (list p) (list body))
        (string-join
         (list
          (format "def init(~a):" p)
          (ir->py body #:indent "  "))
         "\n")])))

  (match-define (parser simple-parse-template _)
    (lang-parser terminals '() compounds '() prim-parsers))
  (define (term->program stx)
    (define ast (simple-parse-template stx))
    (tc-temps/expecteds (list ast) (list (syntax-e c-id)) '())
    (define ir (compile-temp
                ast 'program_ast
                (ir:let (list (list 'result (ir:call-builtin 'run (list 'program_ast))))
                        (ir:return (list 'result)))))
    (string-join
     (list
      "def main():"
      (ir->py ir #:indent "  "))
     "\n"))
  (values print-interpreter term->program))

