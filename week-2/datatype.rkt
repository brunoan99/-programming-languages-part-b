#lang racket

(provide (all-defined-out))

#| Datatype-Programmion in Racket Without Structs |#

#| Life without datatypes

Racket has nothing like a datatype binding for one-of types

No need i a dynamically typed language>
  - Can just mix values of different types and use primitives like number?, string?, pair?, etc.
  - Can use cons cells to build up any kind of data

|#

#| Mixed collections

In ML, cannot have a list of "ints or strings," so use a datatype:

```sml
  datatype int_or_string = I of int | S of string

  fun funny_sum xs = (* int_or_string list -> int *)
    case xs of
        []         => 0
      | (I i)::xs' => i + funny_sum xs'
      | (S s)::xs' => String.size s + funny_sum xs'
```

In Racket, dynamic typing makes this natural without explicity tags
  - Instead, every value has a tag with primitives to check it
  - So just check car of list with number? or string?

|#

(define (funny-sum xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (funny-sum (cdr xs)))]
        [(string? (car xs)) (+ (string-length (car xs)) (funny-sum (cdr xs)))]))
        ; arguably bad style to not have else|#t clause

#| Recursive structures

More interesting datatype-programming we know:

```sml
  datatype exp = Const of int
               | Negate of exp
               | Add of exp * exp
               | Multiply of exp * exp

fun eval_old (Constant i)       = i
  | eval_old (Negate e1)        = ~ (eval_old e1)
  | eval_old (Add (e1,e2))      = (eval_old e1) + (eval_old e2)
  | eval_old (Multiply (e1,e2)) = (eval_old e1) * (eval_old e2)

val test_exp = Multiply (Negate (Add (Const 2, Const 2)), Const 7)

val old_test = eval_exp_old test_exp (* -> ~28 )

exception Error of string

fun eval_new e =
  let
    fun get_int e =
      case e of
          Const i => i
        | _ => raise (Error "expected Const result")
  in
    case e of
        Const _           => e
      | Negate e2         => Const (~ (get_int (eval_new e2)))
      | Add (e1, e2)      => Const ((get_int (eval_new e1)) + (get_int (eval_new e2)))
      | Multiply (e1, e2) => Const ((get_int (eval_new e1)) * (get_int (eval_new e2)))
  end

val new_test = eval_exp_new test_exp (* -> Const ~28 *)

```

|#

#| Change how we do this

- Previous version of eval_exp has type exp -> int

- From now on will write such functions with type exp -> exp

- Why? Because will be interpretingh languages with multiple kinds of results (ints, pairs, functions,...)
  - Even though much more complicated for example so far

- How?
  - Base case returns entire expression, e.g., (Const 17)
  - Recursive cases:
    - Check variant (e.g., make sure a Const)
    - Extract data (e.g., the number under the Const)
    - Also return an exp (e.g., create a new Const)

|#

#| Now implementing the same idea in Racket

datatype exp = Const of int | Negate of exp | Add of exp * exp | Multiply of exp * exp

just helper functions that make lists where first element is a symbol
|#
(define (Const i) (list 'Const i))
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2))
(define (Multiply e1 e2) (list 'Multiply e1 e2))

#| just helper functions that test what "kind of exp"
  Note: More robust could raise better errors for non-exp values
|#
(define (Const? x) (eq? (car x) 'Const))
(define (Negate? x) (eq? (car x) 'Negate))
(define (Add? x) (eq? (car x) 'Add))
(define (Multiply? x) (eq? (car x) 'Multiply))

#| just helper functions that get the pieces for "one kind of exp"
  Note: More robust could check "what kind of exp" |#
(define (Const-int e) (car (cdr e)))
(define (Negate-e e) (car (cdr e)))
(define (Add-e1 e) (car (cdr e)))
(define (Add-e2 e) (car (cdr (cdr e))))
(define (Multiply-e1 e) (car (cdr e)))
(define (Multiply-e2 e) (car (cdr (cdr e))))

#| fyi: there are built-in function for getting 2nd, 3rd list elements the have made the above simpler:
(define Const-int cadr)
(define Negate-e cadr)
(define Add-e1 cadr)
(define Add-e2 caddr)
(define Multiply-e1 cadr)
(define Multiply-e2 caddr)

  same recursive structure as we have in ML, just without pattern-matching and one change from what we did before:
    returning an exp, in particular a Constant, rather than an int
|#
(define (eval-exp e)
  (cond [(Const? e) e]
        [(Negate? e)    (let ([v (Const-int (eval-exp (Negate-e e)))])
                          (Const (- v)))]
        [(Add? e)       (let ([v1 (Const-int (eval-exp (Add-e1 e)))]
                              [v2 (Const-int (eval-exp (Add-e2 e)))])
                          (Const (+ v1 v2)))]
        [(Multiply? e)  (let ([v1 (Const-int (eval-exp (Multiply-e1 e)))]
                              [v2 (Const-int (eval-exp (Multiply-e2 e)))])
                          (Const (* v1 v2)))]
        [#t (error "eval-exp expected an exp")]))
  ; cond is very useful to get used like pattern-matching

#| New way in Racket

Key points:
  - Define our own constructor, test-variant, extrac-data functions,
    - Just better style than hard-to-read uses of car, cdr
  - Same recursive structure without pattern-matching
  - With no type system, no notion of "what is an exp" except in documentation
    - But if we use the helper functions correctly, then okay
    - Could add more explicit error-checking if desired

|#

#| Symbols

Will not focus on Racket symbols like 'foo
  - Syntatically start with quote character
  - Like strings, can be almost any character sequence
  - Unlike strings, compare two symbols with eq? which is fast

|#
