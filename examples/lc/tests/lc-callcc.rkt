#lang racket
(let-values ([() (call/cc (lambda (k) (k)))])
  #t)
(let-values ([(x y z) (call/cc (lambda (k) (k 1 2 3)))])
  (equal? (+ x y z) 6))
(equal? (+ 1 2 3 (call/cc (lambda (k) (k -6)))) 0)
