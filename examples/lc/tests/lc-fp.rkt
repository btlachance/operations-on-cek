#lang racket
(equal? (+ 1. 1.) 2.)
(define (fp-sumhalves n)
  (print n)
  (newline)
  (if (= n 0.0)
      0.0
      (+ n (fp-sumhalves (/ n 2.0)))))
(fp-sumhalves 100.0)
