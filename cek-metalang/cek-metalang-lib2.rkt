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
 "prims.rkt"
 (for-template racket/base))
(provide -production -step compile-cek)

(struct production (name forms) #:prefab)
(define-syntax-class -production
  #:attributes (name (form 1) data)
  #:datum-literals (::=)
  (pattern (name:id ::= form ...+)
           #:attr data (production #'name (attribute form))))

(struct step (lhs rhs wheres) #:prefab)
(define-splicing-syntax-class where-clause
  #:attributes (body)
  (pattern (~seq #:where pattern template)
           #:attr body (list #'pattern #'template)))
(define ((mk/parse-where* parse-temp parse-pat) w)
  (match w
    [(list pattern template)
     (where* (parse-temp template) (parse-pat pattern))]))
(define-syntax-class -step
  #:attributes (lhs rhs (wheres 1) data)
  #:datum-literals (-->)
  (pattern [(~and lhs (e-l env-l k-l))
            -->
            (~and rhs (e-r env-r k-r))
            wheres:where-clause ...]
           #:attr data (step #'lhs #'rhs (attribute wheres.body))))

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
  (define variable-prim variable)
  (define nonterminals
    ;; HACK Need to detect whether the grammar actually uses the
    ;; variable and actually insert the nonterminal into this list
    ;; like we do for other nonterminals
    (append (map (compose nt production-name) productions)
            (list (nt (prim-data-name variable-prim)))))
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
       (prim (syntax-e this-syntax) variable-prim)]
      [_ #f]))
  (lang-info nonterminals
             ;; hard-coded metafunctions for now...
             (list (list 'lookup (nt 'env) (nt 'var))
                   (list 'extend (nt 'env) (nt 'var) (nt 'w)))
             ;; TODO hard-code the environment prim
             (list (parser variable-parse-fun variable-parse-fun))
             sort->name sort->field-names sort->type
             (hash 'lookup 'w
                   'extend 'env)
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

;; compile-cek : id (listof production) id id id (listof step)
;;                 -> (values (-> (void)) (stx -> string))
(define (compile-cek lang-id productions c-id e-id k-id steps final)
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
  (define-values (compile-temp compile-pat compile-where*s)
    (lang-compiler sort->field-names sort->name))

  ;; a method is a (method symbol (listof symbol) IR)
  (struct method (class-name arg-names body) #:transparent)
  (define (step->methods step)
    (match-define (list c0-ast e0-ast k0-ast)
      (stx-map parse-pat (step-lhs step)))
    (define where*s (map (mk/parse-where* parse-temp parse-pat) (step-wheres step)))
    (match-define (list c*-ast e*-ast k*-ast)
      (stx-map parse-temp (step-rhs step)))

    (define cek/tys (map syntax-e (list c-id e-id k-id)))
    (tc-ast*s
     (append
      (map pat* (list c0-ast e0-ast k0-ast) cek/tys)
      where*s
      (map temp* (list c*-ast e*-ast k*-ast) cek/tys)))

    (if (or (metavar? k0-ast)
            (prim? k0-ast))
        (list
         (method
          (ast->name c0-ast)
          (list 'self 'e 'k)
          (foldr
           compile-pat
           (compile-where*s
            where*s
            (foldr
             compile-temp
             (ir:return (list 'c_result 'e_result 'k_result))
             (list c*-ast e*-ast k*-ast)
             (list 'c_result 'e_result 'k_result)))
           (list c0-ast e0-ast k0-ast)
           (list 'self 'e 'k))))
        ;; TODO this branch assumes that the c and e patterns
        ;; are only metavar patterns; check this assumption and
        ;; raise an error if it doesn't hold
        (list
         (method
          (ast->name c0-ast)
          (list 'self 'e 'k)
          (foldr
           compile-pat
           (ir:send 'k (map metavar->symbol (list c0-ast e0-ast)))
           ;; We can insert the metavar k here because we know
           ;; it's the name of one of the arguments.
           (list c0-ast e0-ast (metavar 'k #f))
           (list 'self 'e 'k)))
         (method
          (ast->name k0-ast)
          (list 'self 'c_arg 'e_arg)
          (foldr
           compile-pat
           (compile-where*s
            where*s
            (foldr
             compile-temp
             (ir:return (list 'c_result 'e_result 'k_result))
             (list c*-ast e*-ast k*-ast)
             (list 'c_result 'e_result 'k_result)))
           (list k0-ast c0-ast e0-ast)
           (list 'self 'c_arg 'e_arg))))))
  (define method-by-class-name
    (for/fold ([method-map (hash)])
              ([m (apply append (map step->methods steps))])
      (match* (m (hash-ref method-map (method-class-name m) #f))
        [((method class-name arg-names0 body0)
          (method _ arg-names1 body1))
         (when (and (not (equal? arg-names0 arg-names1))
                    (not (equal? body0 body1)))
           (error 'compile-cek "class ~a has two methods that are not equivalent\n  method 1: ~a\n  method 2: ~a"
                  class-name
                  body0
                  body1))]
        [(_ #f)
         (void)])
      (hash-set method-map (method-class-name m) m)))

  ;; check-for-super-method : name (U name 'top) -> (U ir:method-def 'super)
  (define (check-for-super-method class-name super-name)
    (let loop ([defining-class super-name])
      (match defining-class
        ['top
         (ir:method-def
          '()
          (ir:error (format "class ~a does not implement a method" class-name)))]
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
          [(method _ args body)
           (ir:method-def args body)]
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
          [(method _ args body)
           (ir:method-def args body)]
          [_
           (check-for-super-method class-name parent-class-name)])))))
  (define (print-interpreter)
    (for ([def (in-sequences nt-class-defs other-class-defs)])
      (pretty-display (class-def->py def))))

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

