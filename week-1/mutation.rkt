#lang racket

(provide (all-defined-out))

#| Mutation with Set |#

#| Set!

Unlike ML, Racket really has assignment statements, so we
  - But used only when really appropriate

  (set! x e)

For the x in the current environment, subsequent lookups of x get the result of evaluating expression e
  - Any code using this x will be affected
  - Like x = e in Java, C, Python, etc

Once you have side-effects, sequences are useful:
  (begin e1 e2 ... en)
|#
(define b 3)
(define f (lambda (x) (* 1 (+ x b))))
(define c (+ b 4))  ; 7
(set! b 5)
(define z (f 4))    ; 9
(define w c)        ; 7
#|
  Not much new here:
    - Environment for closure determination when function is defined, but body is evaluated when function is called
    - Once an expression produces a value, it is irrelevant how the value was produced

|#

#| Top-level

- Mutation top-level definition is particularly problematic
  - What if any code could do set! on anything?
  - How could we defend against this?

- A general principle: If something you need not to change might change, make a local copy of it
Example
|#
(define b1 3)
(define f1
  (let ([b1 b1])
    (lambda (x) (* 1 (+ x b1)))))
#|
  Could use a different name for local copy but do not need to

  Simple elegant language desing:
    - Promitives like + and * are just predefined variables bound to functions
    - But maybe that means they are mutable
|#
(define b2 4)
(define f2
  (let ([b2 b2]
        [+ +]
        [* *])
      (lambda (x) (* 1 (+ x b2)))))
#|
    - Even that won't work if f uses other functions that use things that might get mutated - all functions would need to copy everything mutable they used

|#

#| No such madness

In Racket, you do not have to program like this
  - Each file is a module
  - If a module does not use set! on a top-level variable, then Racket makes it constant and forbids set! outside the module
  - Primitives like +, *, and cons are in a module that does not mutate them

Showed this for the concept of copying to defend against mutation
  - Easier defense: Do not allow mutation
  - Mutable top-level binings a highly dubious idea

|#
