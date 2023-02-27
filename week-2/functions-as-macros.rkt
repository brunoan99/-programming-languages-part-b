#lang racket

(provide (all-defined-out))

(require "implementing-languages.rkt")

#| Racket Functions As "Macros" For Interpreted Language |#

#| Recall...

Our approach to language implementation:
  - Implementing language B in language A
  - Skipping parsing bty writing language B program directly in terms of language A constructors
  - An interpreter written in A recursively evaluates

What we know about macros:
  - Extend the syntax of a language
  - Use of a macro expands into language syntax before the program is run, i.e., before calling
  the mais interpreter function
|#

#| Put it together

With our set-up, we can use language A (i.e., Racket) functions that produce language B abstract syntax
 as language B "macros"
  - Language B programs can use the "macros" as though they are part of language B
  - No change to the interpreter or struct definitions
  - Just a programming idiom enabled by our set-up
    - Helps teach what macros are
  - See code for example "macro" definitions and "macro" uses
    - "macro expansion" happens before calling eval-exp

|#

(define (andalso e1 e2)
  (if-then-else e1 e2 (bool #f)))

(define (double e)
  (multiply e (const 2)))

(define (list-product es)
  (if (null? es)
      (const 1)
      (multiply (car es) (list-product (cdr es)))))

(define test (andalso (eq-num (double (const 4))
                                      (list-product (list (const 2)
                                                          (const 2)
                                                          (const 1)
                                                          (const 2))))
                      (bool #t)))
