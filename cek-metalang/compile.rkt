#lang racket
(require "rep.rkt" "ir.rkt" "util.rkt" racket/syntax)
(provide lang-compiler)

;; lang-compiler : (hash sort (listof symbol)) (hash sort symbol)
;;                                      -> (values (ast dest IR -> IR)
;;                                                 (ast source IR -> IR))
(define (lang-compiler sort->field-names sort->name [... #f])
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
       ((prim-data-compile-temp p dest rest))]
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
       (ir:check-instance
        source s
        rest)]
      [(metavar nt suffix)
       (ir:check-instance
        source nt
        (ir:let (list (list (metavar->symbol ast) source))
                rest))]
      [(prim p data)
       ((prim-data-compile-pat p source rest))]
      [(compound asts sort)
       (define projection-dests
         (for/list ([field-name (hash-ref sort->field-names sort)])
           (format-symbol "~a_~a" source field-name)))

       (ir:check-instance
        source (hash-ref sort->name sort)
        (ir:let (for/list ([dest projection-dests]
                           [field-name (hash-ref sort->field-names sort)])
                  (list dest (ir:project (hash-ref sort->name sort) field-name source)))
                (foldr
                 compile-pat
                 rest
                 asts
                 projection-dests)))]))
  (values compile-temp compile-pat))

;; TODO Think about what typechecking and compiling have in
;; common. One commonality: terminal patterns don't bind any
;; variables, thus the generated IR doesn't need to introduce any
;; variables
