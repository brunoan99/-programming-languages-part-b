#lang racket

(provide (all-defined-out))

#| Optional: Variables, Macros and Hygiene |#

#| Another bad macro

Any function that doubles it argument is fine for clients
|#
(define (dbl1 x) (+ x x))
(define (dbl2 x) (* 2 x))
#|
  - These are equivalent to each other

So macros for doubling are bad style but instructive examples:
|#
(define-syntax dbl3 (syntax-rules() [(dbl x) (+ x x)]))
(define-syntax dbl4 (syntax-rules() [(dbl x) (* 2 x)]))
#|
  These are not equivalent to each other. Consider
|#

#| More examples

Sometimes a macro should re-evaluate an argument it is passed
  - If not, as in dbl, then use a local binding as needed:
|#
(define-syntax dbl5
  (syntax-rules ()
    [(dbl5 x)
     (let ([y x]) (+ y y))]))
#|
  Also good style for macros not to have surprising evaluation order
    - Good rule of thumb to preserve left-to-right
    - Bad example
|#
(define-syntax take
  (syntax-rules (from)
    [(take e1 from e2)
      (- e2 e1)]))
#|
  Strange evaluation order
    e1 is written first in macro syntatic sugar expression
    e1 is evaluated after e2 in macro execution
|#

#| Local variables in macros

In C/C++, defining local variables inside macros is unwise
  - When needed done with hacks likje __strange_name34

Here is why with a silly example:
  - Macro:
|#
(define-syntax dbl6
  (syntax-rules ()
    [(dbl6 x) (let ([y 1])
                (* 2 x y))]))
#|
  - Use:
|#
;   (let ([y 7]) (dbl6 y))
#|
  Naive expansion:
|#
;   (let ([y 7]) (let ([y 1])
;                     (* 2 y y)))
#|
  But instead Racket "gets it right", which is part of hygiene
  Racket not goes to Naive expansion result
|#


#| The other side of hygiene

This also looks like it would do the "wrong" thing
  - Macro:
|#
(define-syntax dbl7
  (syntax-rules ()
    [(dbl x) (* 2 x)]))
#|
  - Use:
|#
;   (let ([* +]) (dbl 42))
#|
  - Naive expansion:
|#
;   (let ([* +]) (* 2 42))
#|
  But again Racket's hygienic macros get this right
  Racket uses environment where the macro was defined, than shadowing some expressions will not take effect to the macro
|#

#| How hygienic macros work

A hygienic macro system:
  1. Secretly renames local variables in macros with fresh names
  2. Looks up variables used in macros where the macro is defined

Neither of theses rules are followed by the "naive expansion" most macro system use
  - Without hygiene, macros are much more brittle (non-modular)

On rare occasions, hygiene is not what you want
  - Racket has somewhat complicated support for that

|#
