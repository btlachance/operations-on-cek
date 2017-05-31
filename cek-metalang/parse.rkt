#lang racket
(require syntax/parse "rep.rkt")
(provide lang-parser)

;; a parse-fun is a (syntax -> (U ast #f))
;; a parser is a (parser parse-fun parse-fun)
(struct parser (parse-template parse-pattern))

;; a prim-parser is like a parser but the parse-fun's have the type
;; (syntax lang -> (U prim #f))

;; symbol->metavar : nt symbol -> (U metavar #f)
(define (symbol->metavar expected sym)
  (match (symbol->string sym)
    [(regexp #px"^([^_]+)(_([^_]+))?$" (list _ nt _ suffix))
     #:when (equal? (symbol->string (nt-symbol expected)) nt)
     (metavar (string->symbol nt) (and suffix
                                       (or
                                        ;; TODO check suffix is a nat
                                        (string->number suffix 10)
                                        (string->symbol suffix))))]
    [_ #f]))
(define-syntax-class (-metavar-of nt)
  #:attributes (data)
  (pattern x:id
           #:attr data (symbol->metavar nt (syntax-e #'x))
           #:when (metavar? (attribute data))))
(define-syntax-class (-metavar nts)
  #:attributes (data)
  (pattern (~and (~fail #:when (empty? nts))
                 (~or (~var mv1 (-metavar-of (car nts)))
                      (~var mv2 (-metavar (cdr nts))))
                 (~bind [data (or (attribute mv1.data) (attribute mv2.data))]))))

(define-syntax-class (-terminal-of terminal)
  #:attributes (data)
  (pattern :id
           #:when (equal? (syntax-e this-syntax) terminal)
           #:attr data terminal))
(define-syntax-class (-terminal terminals)
  #:attributes (data)
  (pattern (~and (~fail #:when (empty? terminals))
                 (~or (~var t1 (-terminal-of (car terminals)))
                      (~var t2 (-terminal (cdr terminals))))
                 (~bind [data (or (attribute t1.data) (attribute t2.data))]))))

(define-syntax-class (-metafunction-of sort parser)
  #:attributes (data)
  (pattern (name args ...)
           #:when (equal? (syntax-e #'name) (car sort))
           #:when (= (length (attribute args)) (length (cdr sort)))
           #:attr data (metafunction (syntax-e #'name) (map parser (attribute args)) sort)))
(define-syntax-class (-metafunction sorts parser)
  #:attributes (data)
  (pattern (~and (~fail #:when (empty? sorts))
                 (~or (~var mf1 (-metafunction-of (car sorts) parser))
                      (~var mf2 (-metafunction (cdr sorts) parser)))
                 (~bind [data (or (attribute mf1.data) (attribute mf2.data))]))))

(define-syntax-class (-prim-of prim-parse-fun)
  #:attributes (data)
  (pattern _
           #:attr data (prim-parse-fun this-syntax)
           #:when (prim? (attribute data))))
(define-syntax-class (-prim prim-parse-funs)
  #:attributes (data)
  (pattern (~and (~fail #:when (empty? prim-parse-funs))
                 (~or (~var prim1 (-prim-of (car prim-parse-funs)))
                      (~var prim2 (-prim (cdr prim-parse-funs))))
                 (~bind [data (or (attribute prim1.data) (attribute prim2.data))]))))

(define-syntax-class (-sub-of atomic-sort parse-fun)
  #:attributes (data)
  (pattern id
           #:when (symbol? atomic-sort)
           #:when (equal? (syntax-e this-syntax) atomic-sort)
           #:attr data atomic-sort)
  (pattern _
           #:when (nt? atomic-sort)
           #:attr data (parse-fun this-syntax)
           #:when (attribute data)))
(define-syntax-class (-compound-of list-sort parse-fun)
  #:attributes (data)
  (pattern ()
           #:when (empty? list-sort)
           #:attr data (compound '() '()))
  (pattern (~and (~fail #:when (empty? list-sort))
                 ((~var hd (-sub-of (car list-sort) parse-fun))
                  .
                  (~var tl (-compound-of (cdr list-sort) parse-fun))))
           #:attr data (compound
                        (append
                         (if (nt? (car list-sort))
                             (list (attribute hd.data))
                             '())
                         (compound-asts (attribute tl.data)))
                        list-sort)))
(define-syntax-class (-compound sorts parse-fun)
  #:attributes (data)
  (pattern (~and (~fail #:when (empty? sorts))
                 (~or (~var c1 (-compound-of (car sorts) parse-fun))
                      (~var c2 (-compound (cdr sorts) parse-fun)))
                 (~bind [data (or (attribute c1.data) (attribute c2.data))]))))

;; lang-parser : (setof symbol) (setof nonterminal) (setof sort)
;;               (setof sort) (listof prim-parser) -> parser
;; invariants to ensure that terminals parse as terminals, metavars
;; parse as metavars, and that things otherwise work correctly:
;;  - the symbols in terminals and nonterminals do not overlap
;;  - all sorts in metafunctions are non-symbol sorts, where each
;;    such sort has a symbol as a first element and that symbol does
;;    not overlap with any of the non/terminals
(define (lang-parser terminals nonterminals compounds
                     metafunctions prim-parsers)
  (define (parse-temp stx)
    (syntax-parse stx
      [(~var t (-terminal terminals))
       (attribute t.data)]
      [(~var mv (-metavar nonterminals))
       (attribute mv.data)]
      [(~var mf (-metafunction metafunctions parse-temp))
       (attribute mf.data)]
      [(~var prim (-prim (map parser-parse-template prim-parsers)))
       (attribute prim.data)]
      [(~and (subtemps ...) (~var c (-compound compounds parse-temp)))
       (attribute c.data)]))
  (define (parse-pat stx)
    (syntax-parse stx
      [(~var t (-terminal terminals))
       (attribute t.data)]
      [(~var mv (-metavar nonterminals))
       (attribute mv.data)]
      [(~var prim (-prim (map parser-parse-pattern prim-parsers)))
       (attribute prim.data)]
      [(~and (subpats ...) (~var c (-compound compounds parse-pat)))
       (attribute c.data)]))
  (parser parse-temp parse-pat))

(module+ test
  (require rackunit)
  ;; TODO So that all metavariables are parsed in one place, by the
  ;; -metavar class, primitives should communicate to the
  ;; using-language their desired nonterminal(s).
  (define var-parse-fun
    ;; TODO What if the prim has subtemplates/subpatterns that need
    ;; parsing?
    (syntax-parser
      [:id
       (prim (syntax-e this-syntax) #'var)]
      [_ #f]))
  (define var-parser (parser var-parse-fun var-parse-fun))
  (define t1-parser (lang-parser (list 'mt)
                                 (list (nt 'e) (nt 'x))
                                 (list (list 'lambda (nt 'x) (nt 'e))
                                       (list (nt 'e) (nt 'e)))
                                 ;; (pick e e)
                                 (list (list 'pick (nt 'e) (nt 'e)))
                                 (list var-parser)))
  (define t1-parse-t (parser-parse-template t1-parser))
  (define t1-parse-p (parser-parse-pattern t1-parser))

  (check-equal? (t1-parse-t #'(lambda x x))
                (compound (list (metavar 'x #f) (metavar 'x #f))
                          (list 'lambda (nt 'x) (nt 'e))))
  (check-equal? (t1-parse-p #'(lambda x e))
                (compound (list (metavar 'x #f) (metavar 'e #f))
                          (list 'lambda (nt 'x) (nt 'e))))
  (check-equal? (t1-parse-t #'(pick e e))
                (metafunction 'pick (list (metavar 'e #f) (metavar 'e #f))
                              (list 'pick (nt 'e) (nt 'e))))
  (check-match (t1-parse-t #'(lambda z z))
               (compound (list (prim 'z _) (prim 'z _))
                         (list 'lambda (nt 'x) (nt 'e)))))
