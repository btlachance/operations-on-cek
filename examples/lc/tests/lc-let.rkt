#lang racket
(define x 10)
(let ([x 0]) (+ x 1))
(let () 5)
(define (f) (values 1 2 3))
(define (g) (f))
(let-values ([(x y z) (g)])
  (+ (+ x y) z))
(let-values ([(x) (values 1)])
  (+ x 10))
