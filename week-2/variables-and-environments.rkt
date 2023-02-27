#lang racket

(provide (all-defined-out))

#| Implementing Variables and Environments |#

#| Dealing with variables

- Interpreters so far have been for languages without variables
  - No let-expressions, functions-with-argumetns, etc.
  - Language in homework has all these things

- An environmenti s a mapping from variables (Racket strings) to values (as defined by the language)
  - Only ever put pairs of strings and values in the environment

- Evaluation takes place in an environment
   - Environment passed as argument to interprete helper function
   - A variable expression looks up the variable in the environment
   - Mos subexpressions use same environment as outer expression
   - A let-expression evaluates its body in a larger environment

|#

#| The Set-up

So now a recursive helper function has all the interesting stuff:
(define (eval-under-env e env)
  (cond ...; case for each kind of expression
  ))       ; expression
  - Recursive calls must "pass down" correct environment

Then eval-exp just calls eval-under-env with same expression and the empty environment

On homework, environments themselves are just Racket lists containing Racket pairs of a string (the MUPL variable name, e.g. "x") and a MUPL value (e.g., (int 17))
  MUPL (Made-up Programming languages)

|#
