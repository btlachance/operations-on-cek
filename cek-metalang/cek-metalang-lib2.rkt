#lang racket
(require
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

(struct grammar-components (terminals nonterminals compounds metafunctions prim-parsers))
(define (productions->components productions)
  (define nonterminals (map (compose nt syntax-e production-name) productions))
  (define (nonterminal? f)
    (memf (lambda (nt) (equal? (nt-symbol nt) f)) nonterminals))

  (define-values (terminals compounds)
    (for/fold ([terminals '()]
               [compounds '()])
              ([form-stx (apply in-sequences (map production-forms productions))])
      (define form (syntax->datum form-stx))
      (cond
        [(nonterminal? form)
         (values terminals compounds)]
        [(symbol? form)
         (values (cons form terminals) compounds)]
        [(list? form)
         (define (compound-form->sort f)
           (for/list ([subform f])
             (if (nonterminal? subform)
                 (nt subform)
                 subform)))
         (values terminals (cons (compound-form->sort form) compounds))]
        [else (raise-arguments-error
               'compile-cek
               "unexpected form"
               "form" form)])))
  (grammar-components terminals nonterminals compounds '() '()))

;; compile-cek : id (listof production) id id id (listof step) -> stx
(define (compile-cek lang-id productions c-id e-id k-id steps)
  (match-define (grammar-components terminals nonterminals compounds _ _)
    (productions->components productions))
  (match-define (parser parse-template parse-pattern)
    (lang-parser terminals nonterminals compounds '() '()))

  
  (pretty-print (parse-pattern #'(clo v_0 e_hello)))

  #'(void))
