#lang racket

(provide (all-defined-out))

#| The Truth About Cons |#

#| The truth about cons

const just makes a pair
  - Often called a cons cell
  - By convention and standard library, lists are nested pairs that eventually end with null
|#
(define pr (cons 1 (cons #t "hi"))) ; '(1 #t . "hi") -> a pair
(define lst (cons 1 (cons #t (cons "hi" null)))) ; '(1 #t "hi") -> a list
(define hi (cdr (cdr pr)))
(define hi-again (car (cdr (cdr lst))))
(define hi-again-shorter (caddr lst))
(define no (list? pr))
(define yes (pair? pr))
(define of-course (and (list? lst) (pair? lst)))
#|
  Passing an improper list to function like length is a run-time error

  So why allow improper lists?
    - Pairs are useful
    - Without static types, why distinguish (e1,e2) and e1::e2

  Style:
    - Use proper lists for collections of unknow size
    - But feel free to use cons to build a pair
      - Though structs (like records) may be better

  Built-in-primitives:
    - list? returns true for proper lists, including the empty list
    - pair? return true for things made by cons
      - All improper and proper lists except the empty list

|#
