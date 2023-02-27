#lang racket

(provide (all-defined-out))

#| Implementing Programming Language |#

#| Typical workflow

concrete syntax (string)
  "(fn x => x + x) 4" -> Parsing -> Possible errors / warnings
                                 -> abstract syntax

abstract syntax (tree) -> Possible errors / warnings
            Call
           /   \
          /     \
    Function    Constant
     /   \          |
    /     \         4
    x      +
          / \
         /   \
        Var  Var
         |    |
         x    x

|#

#| Interpreter or compiler

So "rest of implementation" takes the abstract syntax tree (AST) and "runs the program" to produce a result

Fundamentally, two approaches to implement a PL B:
  - Write an interpreter in another language A
    - Better names: evaluator, executor
    - Take a program in B and produce an answer (in B)

  - Write a compiler in another language A to a thrid language C
    - Better name: translator
    - Translation must preserve meaning (equivalence)

  We call A the metalanguage
    - Crucial to keep A and B straight

|#

#| Really more complicated

Evaluation (interpreter) and translation (compiler) are your options
  - But in modern practice have both and multiple layers

A plausible example:
   - Java compiler to bytecode intermediate language
   - Have an interpreter for bytecode (itself in binary), but compile frequent functions to binary at run-time
   - The chip is itself an interpreter for binary
    - Well, except these days the x86 has a translator in hardware to more primitive micro-operations it then executes

Racket uses a similar mix

|#

#| Skipping parsing

- If implementing PL B in PL A, we can skip parsing
   - Have B programmers write ASTs directly in PL A
   - Not so bad with ML constructors or Racket structs
   - Embeds B programs as trees in A

; define B's abstract syntax
(struct call ...)
(struct function ...)
(struct var ...)
...

; example B program
(call (function (list "x")
                (add (var "x")
                     (var "x")))
    (negate (const 4)))

|#

#| Example

- Let the mettalanguag A = Racket
- Let the language-implemented B = "Arithmetic Language"
- Arithmetic programs written with calls to Racket constructors
- The interpreter is eval-exp
|#
(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)

(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e)    (let ([v (const-int (eval-exp (negate-e e)))])
                          (const (- v)))]
        [(add? e)       (let ([v1 (const-int (eval-exp (add-e1 e)))]
                              [v2 (const-int (eval-exp (add-e2 e)))])
                          (const (+ v1 v2)))]
        [(multiply? e)  (let ([v1 (const-int (eval-exp (multiply-e1 e)))]
                              [v2 (const-int (eval-exp (multiply-e2 e)))])
                          (const (* v1 v2)))]
        [#t (error ("eval-exp expected an exp"))]))
