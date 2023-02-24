#lang racket

(provide (all-defined-out))

#| Using Streams |#

#| Streams

- A stream is an infinite sequence of values
  - So cannot make a stream by making all the values
  - Key idea: Use a thunk to delay creating most of the sequence
  - Just a programming idiom

A powerful concept for division of labor:
  - Stream producer knows how create any number of values
  - Stream consumer decides how many values to ask for

Some examples of stream you might (not) be familiar with:
  - User actions (mouse cliks, etc.)
  - UNIX pipes: cmdl1 | cmd2 | has cmd2 "pull" data from cmd1
  - Output values from a sequential feedback circuit

|#

#| Using streams

We will represent streams using pairs and thunks

Let a stream be a thunk that when called returns a pair:
  '(next-answer . next-thunk)

So given a stream st, the client can get any number of elements
  - First:  (car (s))
  - Second: (car ((cdr (s))))
  - Third:  (car ((cdr ((cdr (s))))))
(Usually bind (cdr (s)) to a variable or pass to a recursive function)

|#

#| Defining Streams
|#
(define ones (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 1))))

(define (stream-maker fn arg)
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))
(define nats2  (stream-maker + 1))
(define powers2 (stream-maker * 2))

#| Using Streams
|#
(define (number-untill stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                  (if (tester (car pr))
                    ans
                    (f (cdr pr) (+ ans 1)))))])
      (f stream 1)))
