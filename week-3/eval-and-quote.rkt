#lang racket

(provide (all-defined-out))

#| Optional: eval and quote |#

#| Eval

Racket, Scheme, LISP, Javascript, Ruby, ... have eval
  - At run-time create some data (in Racket a nested list, in javascript a string) however you want
  - Then treat the data as a program and run it
  - Sinc ewe do not know ahead of time what data will be created, we need a language implementation at run-time to support eval
    - Could be interpreter, compiler, combination
    - But do need to "ship a language implementation" in any program containing eval

|#

#| eval in Racket

Appropriate idioms for eval are a matter of contention
  - Often but not always there is a better way
  - Programs with eval are harder to analyze

- It works on nested lists of symbols and other values
- Get advantage from concrete/abstract syntax similarity
|#

(define (make-some-code y)
  (if y
      (list 'begin (list 'print "hi") (list '+ 4 2))
      (list '+ 5 3)))

(eval (make-some-code #t)) ; prints "hi", result 6

#| Quote

- Quoting (quote ...) or '(...) is a special form that makes "everything underneath" atoms and lists, not variables and calls
|#
(list 'begin (list 'print "hi") (list '+ 4 2))
; ==
(quote (begin (print "hi") (+ 4 2)))
#|
  - But the calling eval on it looks up symbols as code
  - So quote and eval are inverses
|#

#|
- There is also quasiquoting
  - Everything underneath is atoms and lists except if unquoted
  - Language like Ruby, Python, Perl eval strings and support putting expressions inside strings, which is quasiquoting
|#
