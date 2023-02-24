#lang racket

(provide (all-defined-out))

#| mcons For Mutable Pairs |#

#| cons cells are immutable

What if you wanted to mutate the contents of a cons cell?
  - In Racket you cannot (major change from Scheme)
  - This is good
    - List-aliasing irrelevant
    - Implementation can make list? fast since listness is determined when cons cell is created
|#
(define x (cons 14 null)) ; x -> '(14)
(define y x) ; 'y -> '(14)
(set! x (cons 42 null)) ; x -> '(42)
; y -> '(14)
(define z x)
; (set! (car x) 45) error in run-time -> set!: not an identifier in: (caar x)
(define mpr (mcons 1 (mcons #t "hi"))) ; mpr -> (mcons 1 (mcons #t "hi"))

(set-mcdr! mpr 47) ; mpr -> (mcons 1 47)
(set-mcdr! mpr (mcons #t "hi")) ; mpr -> (mcons 1 (mcons #t "hi"))
(set-mcar! (mcdr mpr) 14) ; mpr -> (mcons 1 (mcons 14 "hi"))

#| mcons cells are mutable

Since mutable pair are sometimes useful

Racket provides them too:
  - mcons
  - mcar
  - mcdr
  - mpair?
  - set-mcar!
  - set-mcdr!

Run-time error to use mcar on a cons cell or car on a mcons cell

|#
