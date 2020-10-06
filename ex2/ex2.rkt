#lang racket #| ★ CSC324 Fall 2019: Exercise 2 ★ |#
#|
Module: ex2
Description: Exercise 2: Recursive Data Types and Abstract Syntax Trees
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Fall 2019

Before starting, please review the exercise guidelines at
<https://www.cs.toronto.edu/~david/csc324/homework.html>.
|#
;-------------------------------------------------------------------------------
; This expression exports functions so they can be imported into other files.
; Don't change it!
(provide tail-calls)


;-------------------------------------------------------------------------------
; ★ Task 2: Detecting tail calls ★
;-------------------------------------------------------------------------------

#|
(tail-calls expr) -> (listof datum?)
  expr: datum? (with the structure defined on the exercise handout)

  Returns a list of function call expressions that are in tail call position
  with respect to the input expression.
|#
(define (tail-calls expr)
  (match* (expr)
    [((cons first rest))
        (match* (first)
          [('if) (list  (second rest)  (third rest))]
          [('and) (last rest)]
          [('or) (last rest)]
          [(procedure) (cons expr '())]
          )
     ]
    [(_) '()]
  )
)


(module+ test
  (require rackunit)
  ; We've provided test cases for the first three syntactic forms described
  ; in the handout. Please add additional test cases, including ones for the
  ; other forms!
  (test-equal? "Atomic value" (tail-calls 3) empty)
  (test-equal? "Simple call" (tail-calls '(+ 1 2)) (list '(+ 1 2)))
  (test-equal? "Nested call"
               (tail-calls '(+ (* 3 4) 2))
               ; NOTE: the outermost expression is in tail-call position,
               ; and it should just be returned in a list directly. Don't
               ; try to evaluate the inner '(* 3 4) -- this is harder to do!
               (list '(+ (* 3 4) 2)))
  (test-equal? "Simple if" (tail-calls '(if (> x 0) (- x 2) (+ x 3)))
               (list '(- x 2)
                     '(+ x 3))))
