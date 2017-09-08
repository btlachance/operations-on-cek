#lang racket
(exact-integer? 10)
(not (exact-integer? "foo"))
(inexact? 10.1)
(not (inexact? 10))
