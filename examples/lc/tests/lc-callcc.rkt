#lang racket
(equal? (+ 1 2 3 (call/cc (lambda (k) (k -6))))
        0)
