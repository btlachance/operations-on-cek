#lang racket
(define (matrix rows cols)
  (define v (make-vector rows))
  (let loop ([cols-filled 0])
    (if (= cols cols-filled)
        v
        (begin
          (vector-set! v cols-filled (make-vector cols))
          (loop (+ 1 cols-filled))))))
(matrix 10 10)
