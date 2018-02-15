#lang racket
(define vs (vector 1 2 3))
(if (equal? (vector-length vs) 3)
    "success"
    "failure")
(if (equal? (vector-ref vs 2) 3)
    "success"
    "failure")
(if (equal? (vector 1 2 3) (vector 1 2 3))
    "success"
    "failure")
(if (not (equal? (vector 4 5 6) (vector 1 2)))
    "success"
    "failure")
