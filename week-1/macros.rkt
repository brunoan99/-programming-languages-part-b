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

; a cosmetic macro -- adds then, else
(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))

; a macro to replace an expression with another one
(define-syntax comment-out
  (syntax-rules ()
    [(comment-out ignore instead) instead]))

; makes it so users don't write the thunk when using my-delay
(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda () e))]))

#| Overuse

Macros often deserve a bad reputation because they are often overused or used when functions would be better

When in doubt, resist defining a macro?

But they can be used well and the optional material should help
|#
