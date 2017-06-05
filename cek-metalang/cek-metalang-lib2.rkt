#lang racket
(require
 racket/hash
 racket/syntax
 syntax/parse
 "rep.rkt"
 "util.rkt"
 "parse.rkt"
 "typecheck.rkt"
 "compile.rkt"
 "ir.rkt"
 "py-from-ir.rkt"
 (for-template racket/base "prims.rkt"))
(provide -production -step compile-cek)

(struct production (name forms))
(define-syntax-class -production
  #:attributes (name (form 1) data)
  #:datum-literals (::=)
  (pattern (name:id ::= form ...+)
           #:attr data (production #'name (attribute form))))

(struct step (lhs rhs wheres))
(define-splicing-syntax-class where-clause
  #:attributes (body)
  (pattern (~seq #:where pattern template)
           #:attr body (list #'pattern #'template)))
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
  (define variable-prim (syntax-local-value #'variable))
  (define nonterminals
    ;; HACK Need to detect whether the grammar actually uses the
    ;; variable and actually insert the nonterminal into this list
    ;; like we do for other nonterminals
    (append (map (compose nt syntax-e production-name) productions)
            (list (nt (prim-data-name variable-prim)))))
  (define nonterminal-form? (form-in-nonterminals? nonterminals))

  (define-values (sort->name sort->field-names sort->type)
    (for/fold ([sort->name (hash)]
               [sort->field-names (hash)]
               [sort->type (hash)])
              ([production-name (map syntax-e (map production-name productions))]
               [form-stxs (map production-forms productions)]
               #:when #t
               [form-stx form-stxs]
               [form (map syntax->datum form-stxs)]
               [idx (in-naturals)]
               #:when (not (nonterminal-form? form))
               ;; HACK And we need to make sure that primitives /in
               ;; general/ aren't treated like literals
               #:when (not (and (identifier? form-stx)
                                (free-identifier=? form-stx #'variable))))
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
             [sub (map syntax-e (filter identifier? (production-forms p)))]
             #:when (nonterminal-form? sub))
    (define super (syntax-e (production-name p)))
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
                                     (list (production #'e (list #'x #'v))
                                           (production #'x (list #'variable))
                                           (production #'v (list #'(lambda x e) #'z))
                                           (production #'z (list #'foo))))))
  (check-true (subtype?-t1 'x 'e))
  (check-false (subtype?-t1 'e 'x))
  (check-true (subtype?-t1 'v 'e))
  (check-true (subtype?-t1 'z 'v))
  (check-true (subtype?-t1 'z 'e)))

;; compile-cek : id (listof production) id id id (listof step) -> stx
(define (compile-cek lang-id productions c-id e-id k-id steps)
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

  (match-define (parser parse-template parse-pattern)
    (lang-parser terminals nonterminals compounds metafunctions prim-parsers))
  (define subtype? (mk/subtype? parent-of))
  (define-values (tc-temp tc-pat)
    (lang-typechecker
     sort->type
     metafunction->type
     subtype?))
  (define-values (compile-temp compile-pat)
    (lang-compiler sort->field-names sort->name))

  (define (tc-pats/expecteds asts expecteds)
    (for/fold ([bs '()])
              ([ast asts]
               [expected expecteds])
      (match (tc-pat ast)
        [(tc-pattern-result ty bindings)
         (if (subtype? ty expected)
             (append bindings bs)
             (raise-user-error
              'compile-cek
              "expected pattern ~a to have type ~a but got ~a" ast expected ty))]
        [_ (raise-user-error
            'compile-cek
            "could not typecheck pattern ~a" ast)])))
  (define (tc-temps/expecteds asts expecteds bindings)
    (for ([ast asts]
          [expected expecteds])
      (match (tc-temp ast bindings)
        [(tc-template-result ty)
         (unless (subtype? ty expected)
           (raise-user-error
            'compile-cek
            "expected template ~a to have type ~a but got ~a" ast expected ty))]
        [_ (raise-user-error
            'compile-cek
            "could not typecheck template ~a" ast)])))

  ;; a method is a (method symbol (listof symbol) IR)
  (struct method (class-name arg-names body) #:transparent)
  (define (step->methods step)
    (syntax-parse (step-lhs step)
      [(c0 e0 k0)
       (define-values (c0-ast e0-ast k0-ast)
         (values (parse-pattern #'c0) (parse-pattern #'e0) (parse-pattern #'k0)))

       (define (parse-where w)
         (match w
           [(list pattern template)
            (list (parse-pattern pattern) (parse-template template))]))
       (define wheres-asts (map parse-where (step-wheres step)))

       (syntax-parse (step-rhs step)
         [(c* e* k*)
          (define-values (c*-ast e*-ast k*-ast)
            (values (parse-template #'c*) (parse-template #'e*) (parse-template #'k*)))

          (define bindings
            (let loop ([bindings (tc-pats/expecteds
                                  (list c0-ast e0-ast k0-ast)
                                  (map syntax-e (list c-id e-id k-id)))]
                       [asts wheres-asts])
              (match asts
                [(cons (list pat-ast temp-ast) asts-rest)
                 (define ty
                   (match (tc-temp temp-ast bindings)
                     [(tc-template-result ty)
                      ty]
                     [_ (raise-user-error
                         'compile-cek
                         "could not typecheck template ~a" temp-ast)]))
                 (loop (append (tc-pats/expecteds (list pat-ast) (list ty)) bindings)
                       asts-rest)]
                [_
                 bindings])))

          (tc-temps/expecteds (list c*-ast e*-ast k*-ast)
                              (map syntax-e (list c-id e-id k-id))
                              bindings)

          (define (compile-wheres asts rest)
            (define (compile-where w idx r)
              (match w
                [(list pat-ast temp-ast)
                 (define tmp (format-symbol "w_tmp~a" idx))
                 (compile-temp
                  temp-ast tmp
                  (compile-pat
                   pat-ast tmp
                   rest))]))
            (foldr
             compile-where
             rest
             asts
             (build-list (length asts) values)))

          (if (or (metavar? k0-ast)
                  (prim? k0-ast))
              (list
               (method
                (ast->name c0-ast)
                (list 'self 'e 'k)
                (foldr
                 compile-pat
                 (compile-wheres
                  wheres-asts
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
                 (compile-wheres
                  wheres-asts
                  (foldr
                   compile-temp
                   (ir:return (list 'c_result 'e_result 'k_result))
                   (list c*-ast e*-ast k*-ast)
                   (list 'c_result 'e_result 'k_result)))
                  (list k0-ast c0-ast e0-ast)
                  (list 'self 'c_arg 'e_arg)))))])]))
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
        #f
        (match (hash-ref method-by-class-name class-name #f)
          [(method _ args body)
           (ir:method-def args body)]
          [_
           (check-for-super-method class-name parent-class-name)])))))
  (for ([def (in-sequences nt-class-defs other-class-defs)])
    (pretty-display (class-def->py def)))

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
  (pretty-display (term->program #'((lam x ((lam y ((lam a (y a))
                                                    (lam b (y b))))
                                            (lam z ((lam q (z q))
                                                    (lam r (z r))))))
                                    (lam w (w w)))))
  #'(void))
