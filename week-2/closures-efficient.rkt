#lang racket

(provide (all-defined-out))

#| Optional: Are Closures Efficient? |#

#| Is that expensive?

- Time to build a closure is tiny: a struct with two fields

- Space to sotre closures might be large if environment is large
  - But environments are immutable, so natural and correct to have lots of sharing, e.g., of list tails
  - Still, end up keeping around bindings that are not needed

- Alternative used in practive: When creating a closure, store a possibly-smaller environment
  holding only the variables that are free variables in the function body
    - Free variables: Variables that occur, not counting shadowed uses of the same variable name
    - A function body would never needed anything else from the environment

|#

#| Free variables examples

(lambda () (+ x y z)) ; {x, y, z}

(lambda (x) (+ x y z)) ; {y, z}

(lambda (x) (if x y z)) ; {y, z}
  this function will only use y or z never both, but the environment should keep both
  cause the idea its to keep everything thats can be used not everthing thats gonna be used

(lambda (x) ([let ([y 0]) (+ x y z))) ; {z}
  y was shadowed by the let expression

(lambda (x y z) (+ x y z)) ; {}

(lambda (x) (+ y (let ([y z]) (+ y y)))) ; {y, z}
  appers that y was shadowed but if we look before the let expression, one call to y
  was done before it was shadowed, than it must be keep to environment

|#

#| Computing free variables

- So does the interpreter have to analyze the code body every time it creates a closure

- No: Before evaluation begins, compute free variables of every function in program and
 store this information with the function

- Compared to naive store-entire-environment approach, building a closure now takes more time
  but less space
    - And time proportional to number of free variables
    - And various optimizations are possible

- Also use a much better data structure for looking up variables than a list

|#

#| Compiling higher-order function

- If we are compiling to a language without closures (like assembly), cannot rely on there
  being a "current environment"

- So compile function by having the translation produce "regular" functions that all take
  an extra explicit argumetn called "environment"

- And fcompiler replaces all uses of free variables with code that looks up the variable using the environment argument
  - Can make these fast operation with some tricks

- Running program still creates closures and every function call passes the closure's environment to the closure's code

|#
