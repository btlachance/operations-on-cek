#lang racket
(define (add . rest)
  (if (null? rest)
      0
      (+ (car rest) (apply add (cdr rest)))))
(add 1 2 3 4 5 6 7)
