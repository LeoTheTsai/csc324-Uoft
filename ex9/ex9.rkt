#lang racket #| ★ CSC324 Summer 2020 Exercise 9 ★ |#
#|
Module: ex9
Description: Exercise 9: Do Notation as a Macro
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Summer 2020
|#

(provide Nothing Just
         safe-div parse-num
         bind return do-maybe)


;-------------------------------------------------------------------------------
; ★ Task 1: Modeling Maybe in Racket ★
;-------------------------------------------------------------------------------
; Our implementation of "Maybe". Note that Nothing returns a "singleton" symbol,
; while Just is a function that returns a list.
(define Nothing 'Nothing)
(define (Just x) (list 'Just x))


#|
(safe-div x y) -> "Maybe integer?"
  x: integer?
  y: integer?

  If y equals 0, returns Nothing.
  Else returns (quotient x y)---wrapped in a Just, of course!
|#
(define (safe-div x y)
  (if (equal? y 0)
      Nothing
      (Just (quotient x y))
      )
  )

#|
(parse-num s) -> "Maybe integer?"
  s: string?

  If an integer can be parsed from s by using `string->number`, then
  succeed with that integer. Else fail and return Nothing.
|#

(define (parse-num s)
  (if (string->number s)
      (Just (string->number s))
      Nothing))



; Equivalents of `return` and `(>>=)` for Maybe. (bind x f) should be
; equivalent to x >>= f in Haskell.
; You may wish to review *pattern-matching* in Racket for this part.
(define (return x) (Just x))
(define (bind x f)
  (match x
    (Nothing Nothing)
    ((list 'Just a) (f a))
    )
  )



;-------------------------------------------------------------------------------
; ★ Tasks 2 and 3: do notation in Racket ★
;-------------------------------------------------------------------------------
(define-syntax do-maybe
  (syntax-rules (do-let <-)
    [(do-maybe <expr>)
     <expr>]
    [(do-maybe (<id> <- <expr>) <next> ...)
     ((lambda ()
        (if (equal? <expr> Nothing)
            Nothing
            (let ([<id> (first (rest <expr>))])
                  (do-maybe <next> ...)))
        ))
     #;(bind <expr> (lambda (<id>) (do-maybe <next> ...)))]
    [(do-maybe (do-let <id> <value>) <next> ...)
     ((lambda ()
        (let ([<id> <value>])
          (do-maybe <next> ...))))]
    [(do-maybe <expr> <next> ...)
     (if (equal? <expr> Nothing)
         Nothing
         (do-maybe <next> ...))
     ]
    )
  )





(module+ test
  (require rackunit)

  ; Sample tests for Task 2
  (test-equal?
   "do expression with just one argument"
   (do-maybe
    (return 324))
   (Just 324))

  (test-equal?
   "Simple do expression using bind"
   (do-maybe
    (x1 <- (parse-num "3"))
    (x2 <- (parse-num "8"))
    (return (+ x1 x2)))
   (Just 11))

  (test-equal?
   "Simple do expression with Nothing"
   (do-maybe
    (x1 <- (parse-num "3"))
    (x2 <- (parse-num "david"))
    (return (+ x1 x2)))
   Nothing)

  ; Sample tests for Task 3
  (test-equal?
   "Do notation without explicit <-"
   (do-maybe
    (Just 1)
    (Just 2)
    (Just 3)
    (Just 4))
   (Just 4))

  (test-equal?
   "Do notation without explicit <-, with a Nothing"
   (do-maybe
    (Just 1)
    (Just 2)
    Nothing
    (Just 4))
   Nothing)

  (test-equal?
   "Using do-let to bind a pure value"
   (do-maybe
    (do-let x 50)
    (do-let y 8)
    (safe-div x y))
   (Just 6))

  (test-equal?
   "Mixing do-let and bind"
   (do-maybe
    (x <- (parse-num "3"))
    (do-let y (+ x 10))
    (return y))
   (Just 13))

  (test-equal?
   "Mixing do-let and bind"
   (do-maybe
    (x <- (parse-num "david"))
    (do-let y (+ x 10))
    (return y))
   Nothing))
