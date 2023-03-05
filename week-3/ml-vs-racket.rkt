#lang racket

(provide (all-defined-out))

#| ML versus Racket |#

#| Key differences

- Racket and ML have much in common

- Key differences
  - Syntax
  - Pattern-matching vs. struct-tests and accessor-functions
  - Semantics of various let-expression

- Biggest difference: ML's type system and Racket's lack thereof*

* There is Typed Racket, which, interacts well with Racket so you can have typed and untyped modules,
  it differs in interesting ways from ML

|#

#| ML from a Racket perspective

- Syntax, etc. aside, ML is like a well-defined subset of Racket

- Many of the programs it disallows have bugs
  (define (g x) (+ x x)) ; ok
  (define (f y) (+ y (car y)))
  (define (h z) (g (cons z 2)))
  - In fact, in what ML allows, never need primitives like 'number?'

- But other programs it disallows may actually want to write
  (define (f x) (if (> x 0) #t (list 1 2)))
  (define xs (list 1 #t "hi"))
  (define y (f (car xs)))

|#

#| Racket from an ML Perspective

One way to describe Racket is that it has "one big datatype"
  - All values have this type

    datatype theType = Int of int | String of string
                    | Pair of theType * theType
                    | Fun of theType -> theType
                    | ...

  - Constructors are applied implicitly (values are tagged)
    - 42 is really like Int 42       -> | inttag | 42 |

  Primitives implicitly check tags and extract data, raising errors for wrong constructors

    fun car v = case v of Pair(a,b) => a | _ => raise ...
    fun pair? v = case v of Pair _ => true | _ => false

|#

#| More on The One Type

- Built-in constructors for "theType": numbers, strings, booleans, pairs, symbols, procedures, etc.

- Each struct-definition creates a new constructor, dynamically adding to "theType"

|#
