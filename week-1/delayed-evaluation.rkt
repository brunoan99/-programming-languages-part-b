#lang racket

(provide (all-defined-out))

#| Delayed Evaluation and Thunks |#

#| Delayed evaluation

For each language construct, the semantics specifies when subexpression get evaluated. In ML, Racket, Java, C:
  - Function argumetns are eager (call-by-value)
    - Evaluated once before calling the function
  - Conditional branches are not eager

It matter: calling factorial-bad never terminates
|#
(define (my-if-bad e1 e2 e3)
  (if e1 e2 e3))

(define (factorial-normal n)
  (if (= n 1)
      1
      (* n (factorial-normal (- n 1)))))

(define (factorial-bad n)
  (my-if-bad (= n 0)
             1
             (* n (factorial-bad (- n 1)))))

; e2 and e3 shoudl be zero-argument function (delays evaluation)
(define (my-if-strange-but-works e1 e2 e3)
  (if e1 (e2) (e3)))

(define (factorial-delayed n)
  (my-if-strange-but-works (= n 0)
                           (lambda () 1)
                           (lambda () (* n (factorial-delayed (- n 1))))))

#| Thunks delay

We know how to delay evaluation: put expression in a function!
  - Thanks to closures, can use all the same variables later

A zero-argument function used to delay evaluation is called a thunk

This works (but it silly tro wrap if like this)

|#

#| The key point

- Evaluate an expression e to get a result:
      e

- A function that when called, evaluates e and returns result
  - Zero-argument function for "thunking"
      (lambda () e)

- Evaluate e to some thunk and then call the thunk
      (e)


- Powerful idioms related to delaying evaluation and/or avoided repeated or unnecessary computations

|#
