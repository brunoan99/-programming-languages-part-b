#lang racket

(provide (all-defined-out))

#| Delay and Force |#

#| Best of both worlds

Assuming some expensive computation has no side effects, ideally we would:
  - Not compute it until needed
  - Remember the answer so future uses complete immediately

Called lazy evaluation

|#

#| Delay and Force
|#
(define (my-delay th)
  (mcons #f th))

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))
#|
  An ADT represented by a mutable pair
    - #f in car means cdr is unevaluated thunk
      - Really a one-of-type: thunk or result-of-thunk
    - Ideally hide representation in a module
|#

#| Using promises

(define (f p)
  (... (if (...) 0 (... (my-force p) ...))
       (if (...) 0 (... (my-force p) ...))
       ...
       (if (...) 0 (... (my-force p) ...))))

(f (my-delay (lambda () e )))
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

#|
  Examples of usage:
|#
(define x1 (lambda () (my-mult 0 (lambda () (slow-add 3 4)))))
(define y1 (lambda () (my-mult 1 (lambda () (slow-add 3 4)))))
(define z1 (lambda () (my-mult 10 (lambda () (slow-add 3 4)))))

(define x2 (lambda () (my-mult 0 (let ([p (my-delay (lambda () (slow-add 3 4)))])
                      (lambda () (my-force p))))))
(define y2 (lambda () (my-mult 1 (let ([p (my-delay (lambda () (slow-add 3 4)))])
                      (lambda () (my-force p))))))
(define z2 (lambda () (my-mult 10 (let ([p (my-delay (lambda () (slow-add 3 4)))])
                      (lambda () (my-force p))))))
(define w2 (lambda () (my-mult 100 (let ([p (my-delay (lambda () (slow-add 3 4)))])
                      (lambda () (my-force p))))))


#| Lessons From Example

- With thunking second argument:
  - Great if first argument 0
  - Okay if first argument 1
  - Worse otherwise

- With precomputing second argument:
  - Okay in all cases

- With thunk that uses a promise for second argument:
  - Great if first argument 0
  - Okay otherwise

|#
