#lang racket
(define (reverse xs reversed-so-far)
  (if (null? xs)
      reversed-so-far
      (reverse (cdr xs) (cons (car xs) reversed-so-far))))
(reverse (cons 4 (cons 3 (cons 2 (cons 1 '())))) '())
