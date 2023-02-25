#lang racket

(provide (all-defined-out))

#| Macros: The Key Points |#

#|
  High-level idea of macros

  Racket's macro system and pitfalls of macros
    - Defining a macro is the section's homework challenge
    - "Macros done right" a key feature of Racket compared to other macro systems (e.g., C/C++)

|#

#| What is a macro

- A macro definition describes how to transform some new syntax into different syntax in the source language

- A macro is one way to implement syntatic sugar
  - "Replace any syntax of the form e1 andalso e2 with if e1 then e2 else false"

- A macro system is a language (or part of larger language) for defining macros

- Macro expansion is the process of rewriting the syntax for each macro use
  - Before a program is run (or even compiled)

|#

#| Using Racket Macros

- If you define a macro m in Racket, then m becomes a new special form:
  - Use (m ...) gets expanded according to definition

- Example definitions:
  - Expand (my-if e1 then e2 else e3)
    to (if e1 e2 e3)
  - Expand (comment-out e1 e2)
    to e2
  - Expand (my-delay e)
    to (mcons #f (lambda () e))

|#

#| Example uses

It is like we added keywords to our language
  - Other keywords only keywords in uses of that macro
  - Syntax error if keywords misused
  - Rewriting ("expansion") happens before execution
|#

#| Define-syntax |#

; a cosmetic macro -- adds then, else
(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))

; a macro to replace an expression with another one
(define-syntax comment-out
  (syntax-rules ()
    [(comment-out ignore instead) instead]))


#| Revisiting delay and force

Recall our definition of promises from earlier
  - Should we use a macro instead to avoid clients' explicit thunk
|#

(define (my-delay1 th)
  (mcons #f th))

(define (my-force1 p)
  (if (mcar p)
     (mcdr p)
     (begin (set-mcar! p #t)
            (set-mcdr! p ((mcdr p)))
            (mcdr p))))

#|
  (f (my-delay1 (lambda () e)))

  (define (f p)
    (... (my-force1 p) ...))
|#

#| A delay macro

- A macro can put an expression under a thunk
  - Delays evaluation without explicit thunk
  - Cannot implement this with a function
- Now client then should not use a thunk (that would double-thunk)
  - Racket's pre-define delay is a similar macro

makes it so users don't write the thunk when using my-delay
|#
(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda () e))]))
#|
  (f (my-delay e))
|#

; really bad because it evaluates e multiple times
(define-syntax my-force-macro1
  (syntax-rules ()
    [(my-force e)
      (if (mcar e)
          (mcdr e)
          (begin (set-mcar! e #t)
                 (set-mcdr! e ((mcdr e)))
                 (mcdr e)))]))

; do *not* do this either because a function is exactly what we want
(define-syntax my-force-macro2
  (syntax-rules ()
    [(my-force e)
     (let ([x e])
      (if (mcar e)
          (mcdr e)
          (begin (set-mcar! e #t)
                 (set-mcdr! e ((mcdr e)))
                 (mcdr e))))]))
; just use my-force as a function

#| Overuse

Macros often deserve a bad reputation because they are often overused or used when functions would be better

When in doubt, resist defining a macro?

But they can be used well and the optional material should help

|#

#| More Macro Examples |#

#|
  1. A for loop for executing a body a fixed number of times
    - Show a macro that purposely re-evaluates some expressions and not others

  2. Allow 0, 1 or 2 local bindings with fewer parens that let*
    - Show a macro with multiple cases

  3. A re-implementation of let* in terms of let
     - Shows a macro taking any number of arguments
     - Shows a recursive macro
|#

#| 1. |#
(define-syntax for
  (syntax-rules (to do)
    [(for lo to hi do body)
      (let ([l lo]
            [h hi])
        (letrec ([loop (lambda (it)
                          (if (> it h)
                              #t
                              (begin body (loop (+ it 1)))))])
          (loop l)))]))

(define (f x) (begin (print "A") x))
(define (g x) (begin (print "B") x))
(define (h x) (begin (print "C") x))
(for (f 7) to (g 11) do (h 9))  #| "A""B""C""C""C""C""C"#t
                                 evaluates (f 7) one time (g 11) one time and the body 5 times
|#
(for (f 11) to (g 7) do (h 9)) #| "A""B"#t
                                  evaluates (f 11) one time (g 7) one time and the body 0 times
|#


#| 2. |#
(define-syntax let2
  (syntax-rules ()
    [(let2 () body)
      body]
    [(let2 (var val) body)
     (let ([var val]) body)]
    [(let2 (var1 val1 var2 val2) body)
     (let ([var1 val1])
      (let ([var2 val2])
        body))]))

#| 3. |#
(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body)
     body]
    [(my-let* ([var0 val0]
               [var-rest val-rest] ...)
              body)
     (let ([var0 val0])
       (my-let* ([var-rest val-rest] ...)
                body))]))
