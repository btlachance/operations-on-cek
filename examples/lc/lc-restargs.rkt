#lang racket
(define (foldl f init args)
  (if (null? args)
      init
      (foldl f (f init (car args)) (cdr args))))
(define (add . rest)
  (foldl + 0 rest))
(add 1 2 3 4 5 6 7)
