#lang racket
(define v (vector 4 5 6))
(vector-set! v 0 0)
(vector-set! v 1 1)
(vector-set! v 2 2)
(if (= (+ (+ (vector-ref v 0) (vector-ref v 1)) (vector-ref v 2)) 3)
    "success"
    "failure")
