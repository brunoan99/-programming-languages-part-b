#lang racket

(provide (all-defined-out))

#| Dynamic Typing |#

#| Dynamic Typing

For now:
  - Frustrating not to catch "little errors" like (n * x) until test function
  - But can use very flexible data structures and code without convincing a type checker that it makes sense

Example:
  - A list that can contain numbers or other lists
  - Assuming lists or numbers "all the way down," sum all the numbers...

|#

(define xs (list 4 5 6))
(define ys (list (list 4 (list 5 0)) 6 7 (list 8) 9 2 3 (list 0 1)))
(define zs (list (list "hi" (list 4)) (list (list 2) #f (list 0)) (list 2 3 4 5)))

(define (sum1 xs)
  (if (null? xs)
   0
   (if (number? (car xs))
       (+ (car xs) (sum1 (cdr xs)))
       (+ (sum1 (car xs)) (sum1 (cdr xs))))))

(define (sum2 xs)
  (if (null? xs)
   0
   (if (number? (car xs))
       (+ (car xs) (sum2 (cdr xs)))
       (if (list? (car xs))
        (+ (sum2 (car xs)) (sum2 (cdr xs)))
        (sum2 (cdr xs))))))
