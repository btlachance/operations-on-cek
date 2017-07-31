#lang racket
(letrec ([even? (lambda (n)
                  (if (zero? n)
                      #t
                      (odd? (sub1 n))))]
         [odd? (lambda (n)
                 (if (zero? n)
                     #f
                     (even? (sub1 n))))])
  (even? 128))

