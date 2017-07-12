#lang racket
(require "rep.rkt" "ir.rkt" "util.rkt" racket/syntax)
(provide lang-compiler check-instance)

;; lang-compiler : (hash sort (listof symbol)) (hash sort symbol)
;;                                      -> (values (ast dest IR -> IR)
;;                                                 (ast source IR -> IR)
;;                                                 ((listof clause) IR -> IR))
(define (lang-compiler sort->field-names sort->name)
  (define (compile-temp ast dest rest)
    (match ast
      [(? symbol? s)
       (ir:let (list (list dest (ir:make s #f)))
               rest)]
      [(metavar nt suffix)
       (ir:let (list (list dest (metavar->symbol ast)))
               rest)]
      [(metafunction name args _)
       (define subtemp-dests
         (for/list ([arg args]
                    [idx (in-naturals)])
           (format-symbol "~a_~a~a" dest name idx)))
       (foldr
        compile-temp
        (ir:let (list (list dest (ir:call-builtin name subtemp-dests)))
                rest)
        args
        subtemp-dests)]
      [(prim p data)
       ((prim-data-compile-temp data) p dest rest)]
      [(compound asts (? symbol? s))
       ;; XXX The ignore terminal means terminals can be control
       ;; strings, and so they need to be singletons
       (ir:let (list (list dest (format-symbol "__~a_sing" s)))
               rest)]
      [(compound asts sort)
       (define subtemp-dests
         (for/list ([name (hash-ref sort->field-names sort)])
           (format-symbol "~a_~a" dest name)))
       (foldr
        compile-temp
        (ir:let (list (list dest (ir:make (hash-ref sort->name sort) subtemp-dests)))
                rest)
        asts
        subtemp-dests)]))

  (define (compile-pat ast source rest)
    (match ast
      [(? symbol? s)
       (check-instance source s rest)]
      [(metavar nt suffix)
       (check-instance
        source nt
        (ir:let (list (list (metavar->symbol ast) source))
                rest))]
      [(prim p data)
       ((prim-data-compile-pat data) p source rest)]
      [(compound asts sort)
       (define projection-dests
         (for/list ([field-name (hash-ref sort->field-names sort)])
           (format-symbol "~a_~a" source field-name)))

       (check-instance
        source (hash-ref sort->name sort)
        (ir:let (for/list ([dest projection-dests]
                           [field-name (hash-ref sort->field-names sort)])
                  (list dest (ir:project (hash-ref sort->name sort) field-name source)))
                (foldr
                 compile-pat
                 rest
                 asts
                 projection-dests)))]))

  (define (compile-clauses clauses rest)
    (define (compile-clause c idx r)
      (define tmp (format-symbol "w_tmp~a" idx))
      (match c
        [(where* temp-ast pat-ast)
         (compile-temp
          temp-ast tmp
          (compile-pat
           pat-ast tmp
           r))]
        [(unless* temp-ast pat-ast)
         (compile-temp
          temp-ast tmp
          (ir:if-match-fails
           (compile-pat
            pat-ast tmp
            (ir:unless-failure))
           r))]))
    (foldr
     compile-clause
     rest
     clauses
     (build-list (length clauses) values)))
  (values compile-temp compile-pat compile-clauses))

(define (check-instance source s then)
  (ir:if
   (ir:is-instance source s)
   then
   (ir:match-failure (format "Expected ~a to be an ~a" source s))))


;; TODO Think about what typechecking and compiling have in
;; common. One commonality: terminal patterns don't bind any
;; variables, thus the generated IR doesn't need to introduce any
;; variables
