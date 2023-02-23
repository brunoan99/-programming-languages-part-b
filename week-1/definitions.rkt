#lang racket

(provide (all-defined-out))

#| Racket Definitions, Functions, Conditionals |#

(define x 3); val x = 3 in ML
(define y (+ x 2)); + is a function

(define cube1
  (lambda (x)
    (* x (* x x))))

(define cube2
  (lambda (x)
    (* x x x)))

(define (cube3 x)
  (* x x x))

(define (pow1 x y)
  (if (= y 0)
    1
    (* x (pow1 x (- y 1)))
  ))

; Currying
(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

(define three-to-the (pow2 3))

#|
  Calling a function will always be like
  (f a1 ... an)
  where f is the function and p1, ..., pn are the arguments

|#
