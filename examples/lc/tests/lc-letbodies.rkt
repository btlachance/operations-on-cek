#lang racket
(let ([x 0]
      [y 1])
  (print x)
  (print y)
  (print (+ x y)))
(let loop ([nums (cons 1 (cons 2 (cons 3 '())))])
  (if (null? nums) (void) (print (car nums)))
  (if (null? nums) (void) (loop (cdr nums))))
