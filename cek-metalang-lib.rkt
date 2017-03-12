#lang racket
(require
 racket/syntax
 syntax/parse
 (for-template racket/base))

(provide production)
(module+ test
  (require rackunit))

(define (out-of-context stx)
  (raise-syntax-error
   'cek-metalang
   "used out of context"
   stx))

(module keywords racket/base
  (require (for-syntax racket/base))
  (define-syntax (keywords stx)
    (syntax-case stx ()
      [(_ id ...)
       #'(begin
           (define-syntax (id stx)
             (raise-syntax-error 'cek-metalang "out of context")) ...
             (provide id) ...)]))
  (keywords ::= -->
            default-var
            default-env lookup extend
            default-mt ::))
(require (for-template 'keywords))

(struct production (;; sexp
                    name
                    ;; (listof sexp)
                    forms)
  #:constructor-name -production
  #:name -production)

(define-syntax-class production
  #:attributes (name (form 1) data)
  #:literals (::=)
  (pattern (name:id ::= form ...+)
           #:attr data (-production (syntax->datum #'name)
                                    (syntax->datum #'(form ...)))))

(struct step (;; (list sexp sexp sexp)
              lhs
              ;; (list sexp sexp sexp)
              rhs)
  #:constructor-name -step
  #:name -step)
(define-syntax-class step
    #:attributes (lhs rhs data)
    #:literals (-->)
    (pattern [(~and lhs (e-l env-l k-l))
              -->
              (~and rhs (e-r env-r k-r))]
             #:attr data (-step (syntax->datum #'lhs)
                                (syntax->datum #'rhs))))

(struct class (;; sexp
               form
               ;; (U class #f) where #f means it's the root class
               superclass
               ;; (listof step)
               steps
               ;; matcher
               matcher))

;; determine-classes : (listof production) -> (dict form class)

;; For now, we make one assumption (at least one---I don't know what
;; else I'm implicitly assuming) about how grammars are written:
;;   - each form occurs in exactly one production
(define (classes productions)
  (define forms (apply append (map production-forms productions)))
  (define form->matcher (matchers forms))
  
  (define metavar-form->class
    (for/hash ([p productions])
      (define name (production-name p))
      (values name (class name #f '() (dict-ref form->matcher name)))))

  (for/fold ([form->class metavar-form->class])
            ([p productions])
    (define superclass (hash-ref form->class (production-name p)))
    (define new-form-entries
      (for/fold ([f->cs '()])
                ([f (production-forms p)])
        (define matcher (dict-ref form->matcher f)
        (define form-class (class f superclass '() matcher))
        `(,f ,form-class . f->cs))))
    (apply hash-set* form->class new-form-entries)))
(module+ test
  (require rackunit)
  (define classes
    (classes (list (-production 'e '(x v (e e)))
                   (-production 'v '((lam x e)))
                   (-production 'x '(default-var)))))
  (check-false (class-superclass (hash-ref classes 'e)))
  (check-equal? (class-superclass (hash-ref classes 'v))
                (hash-ref classes 'e))
  (check-equal? (class-superclass (hash-ref classes '(lam x e)))
                (hash-ref classes 'v))
  (check-equal? (class-superclass (hash-ref classes 'x))
                (hash-ref classes 'e))

  (define (is-root? c) (not (class-superclass c)))
  (check-equal? (length (filter is-root? (dict-values classes))) 1))

;; build-matchers : (listof form) -> (dict form matcher)
(define (matchers forms)
  (define base-matchers (hash 'default-var symbol?))
  (hash))
  

;; (listof class) (listof step) -> (listof class)
;; produces a list of class like classes, but each step in steps is
;; added to exactly one class in the resulting list
(define (associate-steps classes steps)
  (for/list ([c classes])
    (define c-steps (filter (step-matches? c) steps))
    (struct-copy class c [steps c-steps])))

(define ((step-matches? class) step)
  (class-matches? (first (step-lhs step))))
  
  
