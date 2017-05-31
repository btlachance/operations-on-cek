#lang racket
(require "rep.rkt" "ir.rkt" racket/syntax)
(provide lang-compiler)

;; The IR currently expects symbols for variable names. Later, though,
;; it might be worth making it use metavar's
(define (metavar->symbol mv)
  (match mv
    [(metavar nt #f) nt]
    [(metavar nt suffix)
     (format-symbol "~a_~a" nt suffix)]))

;; lang-compiler : (sort -> (listof symbol)) (sort -> symbol)
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
      [(prim _ _)
       ...]
      [(compound asts sort)
       (define subtemp-dests
         (for/list ([name (sort->field-names sort)])
           (format-symbol "~a_~a" dest name)))
       (foldr
        compile-temp
        (ir:let (list (list dest (ir:make (sort->name sort) subtemp-dests)))
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
      [(prim _ _)
       ...]
      [(compound asts sort)
       (define projection-dests
         (for/list ([field-name (sort->field-names sort)])
           (format-symbol "~a_~a" source field-name)))

       (ir:check-instance
        source (sort->name sort)
        (ir:let (for/list ([dest projection-dests]
                           [field-name (sort->field-names sort)])
                  (list dest (ir:project (sort->name sort) field-name source)))
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
