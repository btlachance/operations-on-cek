#lang racket
(define (fib n)
  (if (zero? n)
      0
      (if (zero? (- n 1))
          1
          (+ (fib (- n 1)) (fib (- n 2))))))
(fib 30)
