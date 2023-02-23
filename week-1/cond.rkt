#lang racket

(provide (all-defined-out))

#| Cond |#

#| Better style

Avoid nested if-expressions when you can use cond-expressions inestead
  - Can think one as sugar for the other

General syntax:
      (cond [e1a e1b]
            [e2a e2b]
            ...
            [eNa eNb])

- Good style: eNa should be #t
 - A default expression cause any other expression eXa evaluates to #t

|#


(define xs (list 4 5 6))
(define ys (list (list 4 (list 5 0)) 6 7 (list 8) 9 2 3 (list 0 1)))
(define zs (list (list "hi" (list 4)) (list (list 2) #f (list 0)) (list 2 3 4 5)))

(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [#t (+ (sum3 (car xs)) (sum3 (cdr xs)))]))

(define (sum4 xs)
  (cond [(null? xs) 0]
        [(string? xs) 0]
        [(boolean? xs) 0]
        [(number? xs) xs]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [(list? xs) (+ (sum3 (car xs)) (sum3 (cdr xs)))]
        [#t 0]))

#| What is true?

For both if and cond , test expression can evaluate to anything
  - It is not an error if the result is not #t or #f

Semantics of if and cond:
  - "Treat anything other than #f as true"
  - (In some languages, other things are false, not in Racket)

This feature makes no sense in a statically typed language

Some consider using this feature poor style, but it can be convenient

|#

(define (count-falses xs)
  (cond [(null? xs) 0]
        [(car xs) (count-falses (cdr xs))]
        [#t (+ 1 (count-falses (cdr xs)))]))
