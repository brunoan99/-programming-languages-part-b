#lang racket

(provide (all-defined-out))

#| Syntax and Parentheses |#

#| Racket syntax

Ignoring a few "bells and whistles"
  Racket has an amazingly simple syntax

A term (anything in the language) is either:
  - An atom, e.g., #t, #f, 34, "hi", null, 4.0, x, ...
  - A special form, e.g., define, lambda, if
    - Macros will let us define our own
  - A sequence of terms in parens: (t1 t2 ... tn)
    - If t1 a special form, semantics of sequence is special
    - Else a function call

Examples:
  (+ 3 (car xs))
  (lambda (x) (if x "hi" #t))

|#

#| Brackets

Can use [anywhere you use (, but must match with ]
  - Will see shortly places where [...] is common style

|#

#| Why is this good?

By parenthesizing everything, converting the program text into a tree representing the program (parsing) is trivial and unambiguous
  - Atoms are leaves
  - Sequences are nodes with elements as childres
  - No other rules

Also makes identation easy

Example:
  (define cube
    (lambda (x)
      (* x x x)))

          define
         /     \
        /       \
      cube     lambda
              /     \
             x       *
                    /|\
                   / | \
                  x  x  x

No need to discuss "operator precedence" (e.g., x + y * z)

|#

#| Parenthesis bias

If you look at HTML for a web page, it takes the same approach:
  -( foo writthen <foo>
  -) written </foo>

|#
