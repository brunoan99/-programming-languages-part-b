#lang racket

(provide (all-defined-out))

#| Implementing Closures |#

#| Higher-order functions

The "magic": How do we use the "right environment" for lexical scope when function may return other functions,
store them in data structures, etc.?

Lack of magic: The interpreter uses a closure data structure (with two parts) to keep the environment it will need to use later
|#
(struct closure (env fun) #:transparent)
#|
  Evaluate a function expression:
    - A function is not a value; a closure is a value
      - Evaluating a function returns a closure
    - Create a closure out of (a) the function and (b) the current environment when the function was evaluated

Evaluate a function call:

Function calls
    (call e1 e2)

- Use current environment to evaluate e1 to a closure
  - Error result is a value that is not a closure
- Use current environment to evaluate e2 to a value
- Evaluates closure's function's body in the closure's environment, extended to:
  - Map the function's argument-name to the argument-value
  - And for recursion, map the function's name to the whole closure

This is the same semantics we learned a few weeks ago "coded up"

Given a closure, the code part is only ever evaluated using the
environment part (extended), not the environment at the call-site
|#
