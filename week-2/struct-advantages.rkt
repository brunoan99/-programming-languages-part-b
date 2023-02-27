#lang racket

(provide (all-defined-out))

#| Advantages of Structs |#

(struct add-struct (e1 e2) #:transparent)

#| Versus |#

(define (add-list e1 e2) (list 'add-list e1 e2))
(define (add-list? e) (eq? (car e) ('add-list)))
(define (add-list-e1 e) (cadr e))
(define (add-list-e2 e) (caddr e))

#|
  This is not a case of syntactic sugar
|#

(define add1 (add-struct 1 2))
(define add2 (add-list 1 2))
#|
  both can look similar, but
|#

#| The key difference

- The result of calling (add-struct x y) is not a list
  - And there is no list for which add? returns #t

- struct makes a new kind of thing: extending Racket with a new kind of data

- So calling car, cdr, or mult-e1 on "an add" is a run-time error
|#
(pair? add1) ; -> #f
(list? add1) ; -> #f
;(add-list? add1) ; -> car: contract violation expected: pair? given: (add-struct 1 2)

(pair? add2) ; -> #t
(list? add2) ; -> #t
(add-struct? add2) ; -> #f

#| List approach is error-prone

- Can break abstraction by using car, cdr, and list-library functions directly on "add expressions"
  - Silenty likely error:
    (define xs (list (add (const 1) (const 4)) ...))
    (car (car xs))

- Can make data with add? wrongly answers #t do
  (cons 'add "I am not an add")

|#

#| Summary of advantages

Struct approach:
  - Is better style and moree concise for defining data types
  - Is about equally convenient for using data types
  - But much better at timely errors when misusing data types
    - Cannot accessor functions on wrong kind of data
    - Cannot confuse tester functions

Struct approach is even better combined with other Racket features not discussed here:
  - The module system lets us hide the constructor function to enforce invariants
    - List-approach cannot hide con from clients
    - Dynamically-typed languages can have abstract types by letting modules define new types!

  - The contract system lets us check invariants even if constructor is exposed
    - For example, fields of "an add" must also be "expressions"

Struct is special

Often we end up learning that some convenient feature could be coded up with other features

Not so with struct definitions:
  - A function cannot introduce multiple bindings
  - Neither functions nor macros can create a new kind of data
    - Result of constructor function returns #f for every other tester function: number?, pair?, other structs' tester functions, etc.

|#
