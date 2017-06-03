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
 (for-template racket/base))
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
                   ;; (hash type (U type #f))
                   parent-of))

(define (productions->lang-info productions)
  (define nonterminals (map (compose nt syntax-e production-name) productions))
  (define nonterminal-form? (form-in-nonterminals? nonterminals))

  (define-values (sort->name sort->field-names sort->type)
    (for/fold ([sort->name (hash)]
               [sort->field-names (hash)]
               [sort->type (hash)])
              ([production-name (map syntax-e (map production-name productions))]
               [form-stxs (map production-forms productions)]
               #:when #t
               [form (map syntax->datum form-stxs)]
               [idx (in-naturals)]
               #:unless (nonterminal-form? form))
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

  (lang-info nonterminals '() '()
             sort->name sort->field-names sort->type
             (mk/parent-of nonterminals productions)))

;; mk/parent-of : (setof nonterminal) (listof production) -> (hash type (U type #f))
;; INV:
;; - only the production-name in productions have a corresponding
;;   nonterminal in nonterminals
;; - each nonterminal occurs at most once on the right-hand side of a
;;   production
;; - no cycles in the productions (e.g. (e ::= x) (x ::= y) (y ::= e))
(define (mk/parent-of nonterminals productions)
  (define nonterminal-form? (form-in-nonterminals? nonterminals))
  
  (define (nt->no-parent nt)
    (cons (nt-symbol nt) #f))
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
       (and (hash-ref parent-of t1)
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
                           parent-of)
    (productions->lang-info productions))
  ;; TODO prim's have a class but metafunctions don't, handle those
  ;; cases carefully
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
     (thunk (error 'metafunction->type))
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

       (syntax-parse (step-rhs step)
         [(c* e* k*)
          (define-values (c*-ast e*-ast k*-ast)
            (values (parse-template #'c*) (parse-template #'e*) (parse-template #'k*)))

          (define lhs-bindings
            (tc-pats/expecteds (list c0-ast e0-ast k0-ast)
                               (map syntax-e (list c-id e-id k-id))))
          (tc-temps/expecteds (list c*-ast e*-ast k*-ast)
                              (map syntax-e (list c-id e-id k-id))
                              lhs-bindings)

          (if (or (metavar? k0-ast)
                  (prim? k0-ast))
              (list
               (method
                (ast->name c0-ast)
                (list 'self 'e 'k)
                (foldr
                 compile-pat
                 (foldr
                  compile-temp
                  (ir:return (list 'c_result 'e_result 'k_result))
                  (list c*-ast e*-ast k*-ast)
                  (list 'c_result 'e_result 'k_result))
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
                 (foldr
                  compile-temp
                  (ir:return (list 'c_result 'e_result 'k_result))
                  (list c*-ast e*-ast k*-ast)
                  (list 'c_result 'e_result 'k_result))
                 (list k0-ast c0-ast e0-ast)
                 (list 'self 'c_arg 'e_arg)))))])]))

  (for-each (compose pretty-print step->methods) steps)
  #'(void))
