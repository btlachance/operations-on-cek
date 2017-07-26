#lang racket
(define (even? n)
  (if (zero? n)
      #t
      (odd? (sub1 n))))
(define (odd? n)
  (if (zero? n)
      #f
      (even? (sub1 n))))
(even? 128)
      
