#lang racket
(require
 racket/hash
 syntax/parse
 "rep.rkt"
 "parse.rkt"
 "typecheck.rkt"
 "compile.rkt"
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

(struct grammar-components (nonterminals sort->type metafunctions prim-parsers))
(define (productions->components productions)
  (define nonterminals (map (compose nt syntax-e production-name) productions))
  (define nonterminal-form? (form-in-nonterminals? nonterminals))

  (define sort->type
    (for/fold ([sort->type (hash)])
              ([production-name (map syntax-e (map production-name productions))]
               [form-stxs (map production-forms productions)])
      (hash-union
       sort->type
       (for/hash ([form (map syntax->datum form-stxs)]
                  #:unless (nonterminal-form? form))
         (cond
           [(symbol? form)
            (values form production-name)]
           [(list? form)
            (define (compound-form->sort f)
              (for/list ([subform f])
                (if (nonterminal-form? subform)
                    (nt subform)
                    subform)))
            (values (compound-form->sort form) production-name)]
           [else (raise-arguments-error
                  'compile-cek
                  "unexpected form"
                  "form" form)])))))
  (println sort->type)
  (grammar-components nonterminals sort->type '() '()))

;; mk/subtype? : (setof nonterminal) (listof production) -> (type type -> boolean)
;; INV:
;; - only the production-name in productions have a corresponding
;;   nonterminal in nonterminals
;; - each nonterminal occurs at most once on the right-hand side of a
;;   production
;; - no cycles in the productions (e.g. (e ::= x) (x ::= y) (y ::= e))
(define (mk/subtype? nonterminals productions)
  ;; a subtype is a (subtype symbol symbol) representing a case in the
  ;; subtype relation.
  (struct subtype* (sub super))

  (define nonterminal-form? (form-in-nonterminals? nonterminals))
  (define subtype-cases
    (for/fold ([subtype-cases '()])
              ([p productions])
      (define super (syntax-e (production-name p)))
      (for/fold ([cases subtype-cases])
                ([sub (map syntax-e (filter identifier? (production-forms p)))]
                 #:when (nonterminal-form? sub))
        (cons (subtype* sub super) cases))))

  ;; parent-of : type -> (U type #f)
  ;; Produces the type that has type as its child, or #f for types
  ;; that have no parent
  (define (parent-of type)
    (for/or ([case subtype-cases])
      (match case
        [(subtype* sub super)
         #:when (equal? sub type)
         super]
        [_ #f])))
  (define (subtype? t1 t2)
    (cond
      [(equal? t1 t2) #t]
      [else
       (and (parent-of t1)
            (subtype? (parent-of t1) t2))]))
  subtype?)
(module+ test
  (require rackunit)
  (define subtype?-t1 (mk/subtype? (list (nt 'e) (nt 'x) (nt 'v) (nt 'z))
                                   (list (production #'e (list #'x #'v))
                                         (production #'x (list #'variable))
                                         (production #'v (list #'(lambda x e) #'z))
                                         (production #'z (list #'foo)))))
  (check-true (subtype?-t1 'x 'e))
  (check-false (subtype?-t1 'e 'x))
  (check-true (subtype?-t1 'v 'e))
  (check-true (subtype?-t1 'z 'v))
  (check-true (subtype?-t1 'z 'e)))

;; compile-cek : id (listof production) id id id (listof step) -> stx
(define (compile-cek lang-id productions c-id e-id k-id steps)
  (match-define (grammar-components nonterminals sort->type _ _)
    (productions->components productions))
  (define-values (terminals compounds) (partition symbol? (hash-keys sort->type)))

  (match-define (parser parse-template parse-pattern)
    (lang-parser terminals nonterminals compounds '() '()))
  (define-values (tc-temp tc-pat)
    (lang-typechecker
     sort->type
     (thunk (error 'metafunction->type))
     (mk/subtype? nonterminals productions)))

  (define pat-ast (parse-pattern #'(clo v_0 env_hello)))
  (pretty-print pat-ast)
  (pretty-print (tc-pat pat-ast))
  (pretty-print (tc-pat (parse-pattern #'(lam x e))))

  #'(void))
