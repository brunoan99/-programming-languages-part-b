#lang racket

(provide (all-defined-out))

#| Parentheses Matter! (Debugging Practice) |#

#| Parentheses matter

Must break on habits for Racket:
  - Do not add/remove parens for no reason
    - Parens are never optional or meaningless

  - In most palces (e) means call e with zero arguments

  - So ((e)) means call e with zero arguments and call the result with zero arguments

Without static typing, often get hard-to-diagnose run-time errors

|#

(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))

(define (fact1 n) (if (= n 0) (1) (* n (fact1 (- n 1)))))
#| 1 in parentheses, racket will call '1' with no arguments and raise an error:
    procedure application: expected procedure, give: 1 (no arguments)
|#

(define (fact2 n) (if (= n 0) (1) (* n (fact (- n 1)))))
#| this seems to work
  cause in recursion the function being called is fact
  than wen fact2 is called with anything greater than zero
  fact is called and return the correct result
  but when fact2 is called with 0
  racket try to execute the branch with a wrong code and raise the error:
    procedure application: expected procedure, give: 1 (no arguments)

|#

#|
  (define (fact3 n) (if = n 0 1  (* n (fact3 (- n 1)))))
  this will no run, racket will verify that the if must contain 3 expressions and in this case is being called with 5
|#

#|
  (define fact3 (n) (if (= n 0) 1 (* n (fact3 (- n 1)))))
  this will no run
|#

(define (fact4 n) (if (= n 0) 1 (* n fact4 (- n 1))))
#| this seems to work if called with 0
  but when is called with anything greater than 0
  racket try to execute (* n fact4 (- n 1))
  and cause fact4 call isnt in parentheses racket treat as a parameter to * call
  racket try to execute and raise the error:
    contract violation
      expected: number?
      given: #<procedure:fact4>

|#

(define (fact5 n) (if (= n 0) 1 (* n ((fact5)) (- n 1))))
#| this seems to work if called with 0
  but when is called with anything greater than 0
  racket try to execute (fact5) in (* n ((fact5)) (- n 1))
  and cause fact5 is in one parentheses more than the correct racket try to call it with zero arguments resulting in the error:
    arity mismatch;
      the expected number of arguments does not match the given number
        expected: 1
        give: 0
|#

(define (fact6 n) (if (= n 0) 1 (n * (fact6 (- n 1)))))
#| this seems to work if called with 0
  but when is called with anything greater than 0
  racket try to execute (n * (fact6 (- n 1)))
  and cause n and * is in correct positions racket try to call n with * and (fact6 (- n 1)) as arguments resulting in the error:
    application: not a procedure;
      expect a procedure that can be applied to arguments
        given: 1

|#
