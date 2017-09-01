#lang racket
(require "ir.rkt" json)
(provide term-ir->json)

(define (term-ir->json ir)
  (jsexpr->string (term-ir->jsexpr ir (hash))))

(define (singleton-name? s)
  (and (symbol? s)
       (match (symbol->string s)
         [(regexp #rx"val_[^_]+_sing") #t]
         [_ #f])))

(define (term-ir->jsexpr ir env)
  (match ir
    [(ir:let (list (list name ir)) body)
     (term-ir->jsexpr
      body
      (hash-set env name (simple-ir->jsexpr ir env)))]
    [(ir:return (list result))
     (hash-ref env result)]))
(define (simple-ir->jsexpr ir env)
  (define (arg->jsexpr p)
    (match p
      [(? symbol? s) (hash-ref env s)]
      [(? string? str) str]
      [(? exact-integer? n) n]
      [(? real? n) n]))
  (match ir
    [(? singleton-name? sing) (hash 'singleton (~a sing))]
    [(? symbol? s) (hash-ref env s)]
    [(ir:call-builtin fn-name args)
     (hash 'call-builtin
           (hash 'fn-name (~a fn-name)
                 'args (map arg->jsexpr args)))]
    ;; #f case never happens because of singletons
    [(ir:make class-name args)
     (hash 'make
           (hash 'class-name (~a class-name)
                 'args (map arg->jsexpr args)))]))

(module+ test
  (require rackunit)
  (check-equal?
   (term-ir->jsexpr (ir:let (list (list 'tmp 'val_mt_sing))
                            (ir:return '(tmp)))
                    (hash))
   (hash 'singleton "val_mt_sing"))
  (check-equal?
   (simple-ir->jsexpr (ir:call-builtin 'mkvariable '("x")) (hash))
   (hash 'call-builtin
         (hash 'fn-name "mkvariable"
               'args '("x"))))
  (check-equal?
   (term-ir->jsexpr
    (ir:let (list (list 'e1 (ir:call-builtin 'mkvariable '("x"))))
            (ir:let (list (list 'e2 (ir:call-builtin 'mkvariable '("x"))))
                    (ir:let (list (list 'app_result (ir:make 'app '(e1 e2))))
                            (ir:return '(app_result)))))
    (hash))
   (hash 'make
         (hash 'class-name "app"
               'args (list (hash 'call-builtin
                                  (hash 'fn-name "mkvariable"
                                        'args '("x")))
                            (hash 'call-builtin
                                  (hash 'fn-name "mkvariable"
                                        'args '("x"))))))))
