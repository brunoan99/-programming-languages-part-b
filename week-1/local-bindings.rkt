#lang racket

(provide (all-defined-out))

#| Local Bindings |#

#| Local bindings

- Racket has 4 ways to define local variables
  - let
  - let*
  - letrec
  - define

- Variety is good: They have different semantics
  - Use the one most convenient for your needs, which helps communicate your intent to people reading the code
    - If any will work, use let
  - Will help us better learn scope and environments

- Like in ML, the 3 kinds of let-expressions can appear anywhere

|#


(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
          (if (> tlans (car xs))
            tlans
            (car xs))
        )]))

#| let

A let expression can bind any number of local variables
  - Notice where all the parentheses are

The expressionas are all evaluated in the environment from before the let-expression
  - Except the body can use all the local variables of course
  - This is not how ML let-expressions work
  - Convenient for things like (let ([x y][y x]))
|#
(define (silly-double1 x)
  (let ([x (+ x 3)]
        [y (+ x 2)]) ; here the x binding defined in line above is not used
      (+ x  y -5 )))


#| let*

Syntactically, a let* expression is a let-expression with 1 more character

The expressions are evaluated in the environment produced from the previous bindings
  - Can repeat bindings (later ones shadoes)
  - This is how ML let-expressions work
|#

(define (silly-double2 x)
  (let* ([x (+ x 3)]
         [y (+ x 2)]) ; here the x binding defined in line above is used
      (+ x y -8 )))


#| letrec

Syntactically, a letrec expression is also the same

The expressions are evaluated in the environment that includes all the bindings
|#
(define (silly-triple x)
  (letrec ([y (+ x 2)]
           [f (lambda(z) (+ z y w x))]
           [w (+ x 7)])
        (f - 9)))
#|
    - Needed for mutual recursion
    - But expressions are still evaluated in order: accessing an uninitialized binding would produce an error
      - Would be bad style and surely a bug
      - Remember function bodies not evaluated until called

  - Letrec is ideal for recursion (including mutual recursion)
|#
(define (silly-mod2 x)
  (letrec
    ([even? (lambda(x) (if (zero? x) #t (odd? (- x 1))))]
     [odd? (lambda(x) (if (zero? x) #f (even? (- x 1))))])
      (if (even? x) 0 1)))
#|
  Do not use later bindings except inside function
    - This example will cause and error if x is not #f
|#
(define (bad-letrec x)
  (letrec ([y z]
           [z 13])
      (if x y z)))


#| define

In cenrtain positions, like the beginning of function bodies, you can put defines
  - For defining local variables, same semantics as letrec
|#
(define (silly-mod3 x)
  (define (even? x) (if (zero? x) #t (odd? (- x 1))))
  (define (odd? x) (if (zero? x) #f (even? (- x 1))))
  (if (even? x) 0 1))
#|
  Local defines usage is preferred Racket style
|#
