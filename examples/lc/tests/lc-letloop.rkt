#lang racket
(let loop ([sum 0]
           [nums (cons 1 (cons 2 (cons 3 '())))])
  (if (null? nums)
      sum
      (loop (+ (car nums) sum) (cdr nums))))
