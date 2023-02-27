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

(define (eval-exp-maths e)
  (cond [(const? e) e]
        [(negate? e)    (let ([v (const-int (eval-exp-maths (negate-e e)))])
                          (const (- v)))]
        [(add? e)       (let ([v1 (const-int (eval-exp-maths (add-e1 e)))]
                              [v2 (const-int (eval-exp-maths (add-e2 e)))])
                          (const (+ v1 v2)))]
        [(multiply? e)  (let ([v1 (const-int (eval-exp-maths (multiply-e1 e)))]
                              [v2 (const-int (eval-exp-maths (multiply-e2 e)))])
                          (const (* v1 v2)))]
        [#t (error ("eval-exp-maths expected an exp"))]))

#| What Your Interpreter Can and Cannot Assume |#

#| What we know

- Define (abstract) syntax of language B with Racket structs
  - B called MUPL in homework
- Write B programs directly in Racket via constructors
- Implement interpreter for B as a (recursive) Racket function

Now, a subtle-but-important distinction:
  - Interpreter can assume input is a "legal AST for B"
    - Okay to give wrong answer or inscrutable error otherwise
  - Interpreter must check that recursive results are the right kind of value
    - Give a good error message otherwise

|#

#| Legal ASTs

- "Trees the interpreter must handle" are a subset of all the trees Racket allows as dynamically typed language
(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)

- Can assume "right types" for struct fields
  - const holds a number
  - negate holds a legal AST
  - add and multiply hold 2 legal ASTs

- Illegal ASTs can "crash the interpreter" - this is fine
(multiply (add (const 3) "uh-oh") (const 4))
(negate -7)

|#

#| Interpreter results

- Our interpretes return expressions, but not any expressions
  - Result should always be a value, a kind of expression that evaluates to itself
  - If not, the interpreter has a bug

- So far, only values are from const, e.g., (const 17)

- But a larger language has more values than just numbers
  - Booleans, strings, etc.
  - Pairs of values (definition of value recursive)
  - Closures
  - ...

|#

#| Example

See code for language that adds, booleans, number-comparison, and conditionals:
|#
(struct bool (b) #:transparent)
(struct eq-num (e1 e2) #:transparent)
(struct if-then-else (e1 e2 e3) #:transparent)
#|
  What if the program is a legal AST, but evaluation of it tries to use the wrong kind of value?
    - For example, "add a boolean"
      - Should detect this and give an error message not in terms of the interpreter impelementation
      - Means checking a recursive result whenever a particular kind of value is needed
      - No need to check if any kind of value is okay
|#

(define test1 (multiply (negate (add (const 2)
                                     (const 2)))
                        (const 7)))
; Is a valid AST and produce an result

(define test2 (multiply (negate (add (const 2)
                                     (const 2)))
                        (if-then-else (bool #f)
                                      (const 7)
                                      (bool #t))))
; Is a valid AST and produce an error cause of the wrong kind of value

(define non-test (multiply (negate (add (const #t)
                                        (const 2)))
                           (const 7)))


(define (eval-exp-wrong e)
  (cond [(const? e) e]
        [(negate? e)
            (let ([v (const-int (eval-exp-wrong (negate-e e)))])
              (const (- v)))]
        [(add? e)
            (let ([v1 (const-int (eval-exp-wrong (add-e1 e)))]
                  [v2 (const-int (eval-exp-wrong (add-e2 e)))])
              (const (+ v1 v2)))]
        [(multiply? e)
            (let ([v1 (const-int (eval-exp-wrong (multiply-e1 e)))]
                  [v2 (const-int (eval-exp-wrong (multiply-e2 e)))])
              (const (* v1 v2)))]
        [(bool? e) e]
        [(eq-num? e)
            (let ([v1 (const-int (eval-exp-wrong (eq-num-e1 e)))]
                  [v2 (const-int (eval-exp-wrong (eq-num-e2 e)))])
              (bool (= v1 v2)))]
        [(if-then-else? e)
            (if (bool-b (eval-exp-wrong (if-then-else-e1 e)))
                (eval-exp-wrong (if-then-else-e2 e))
                (eval-exp-wrong (if-then-else-e3 e)))]
        [#t (error ("eval-exp-wrong expected an exp"))]))

(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e)
            (let ([v (eval-exp (negate-e e))])
              (if (const? v)
                  (const (- (const-int v)))
                  (error "negate applied to non-number")))]
        [(add? e)
            (let ([v1 (eval-exp (add-e1 e))]
                  [v2 (eval-exp (add-e2 e))])
              (if (and (const? v1) (const? v2))
                  (const (+ (const-int v1) (const-int v2)))
                  (error "add applied to non-number")))]
        [(multiply? e)
            (let ([v1 (eval-exp (multiply-e1 e))]
                  [v2 (eval-exp (multiply-e2 e))])
              (if (and (const? v1) (const? v2))
                  (const (* (const-int v1) (const-int v2)))
                  (error "multiply called to non-number")))]
        [(bool? e) e]
        [(eq-num? e)
            (let ([v1 (eval-exp (eq-num-e1 e))]
                  [v2 (eval-exp (eq-num-e2 e))])
              (if (and (const? v1) (const? v2))
                  (bool (= (const-int v1) (const-int v2)))
                  (error "eq-num applied to non-number")))]
        [(if-then-else? e)
            (let ([v1 (eval-exp (if-then-else-e1 e))])
              (if (bool? v1)
                  (if (bool-b v1)
                      (eval-exp (if-then-else-e2 e))
                      (eval-exp (if-then-else-e3 e)))
                  (error "if-then-else applied to non boolean")))]
        [#t (error ("eval-exp expected an exp"))]))
