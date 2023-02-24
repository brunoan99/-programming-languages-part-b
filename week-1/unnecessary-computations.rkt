#lang racket

(provide (all-defined-out))

#| Avoiding Unnecessary Computations |#

#| Avoinding expensive computations

Thunks let you skip expensive computations if they are not needed

Great if you take the true-branch

  (define (f th)
    (if (...) 0 (... (th) ...)))

But workse if you end up using the thunk more than once

  (define (f th)
    (... (if (...) 0 (... (th) ...))
         (if (...) 0 (... (th) ...))
         ...
         (if (...) 0 (... (th) ...))))

In general, might not know how many times a result is needed
|#
(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 500000000) y)))

(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk ))]))

#| Best of both worlds

Assuming some expensive computation has no side effects, ideally we would:
  - Not compute it until needed
  - Remember the answer so future uses complete immediatelty
  Called lazy evaluation

Languages where most constructs, including function arguments, work this way are lazy languages
  - Haskell

Racket predefines support for promises, but we can make our own
  - Thunks and mutable pairs are enough

|#
