#lang racket

#| Racket

Like ML, functional focus with imperative features
  - Anonymous functions, closures, no return statements, etc.
  - But without pattern-matching

Unline ML, no static type system: accepts more programs, but most errors do not occur until run-time

Really minimalist syntax

Advanced features like macros, modules, quoting/eval, continuations, contracts,...

|#


#| Racket vs. Scheme

Scheme and Racket are very similar languages
  - Racket "changed its name" in 2010
  - Until this change its treated as a Scheme dialect

Racket made some non-backward-compatible changes
  - How the empty list is written
  - Cons cells not mutable
  - How modules work
  ... and many additions

|#

#| Getting started |#

#| All first non-comment line should be #lang racket |#

#|
  Racket has a module-system, in this module-system all files are modules, and by default anything is private.
  And have to define what will be public and available.
  (provide (all-defined-out)) it's used to make everything public and available.
|#
(provide (all-defined-out))

; basic definitions
(define s "hello")

#|
  Syntax:
    (define v1 e1)

  v1 is the variable name that is being defined
  e1 is the expression that is being stored in the variable
  define is a keyworkd
|#
