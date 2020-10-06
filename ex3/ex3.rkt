#lang racket #| ★ CSC324 Fall 2019: Exercise 3 ★ |#
#|
Module: ex3
Description: Exercise 3: More with Higher-Order Functions; Building Environments
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Fall 2019

Before starting, please review the exercise guidelines at
<https://www.cs.toronto.edu/~david/csc324/homework.html>.
|#
;-------------------------------------------------------------------------------
(provide build-env curry-2 fix-first curry-n)

(module+ test
  (require rackunit))

;-------------------------------------------------------------------------------
; ★ Task 1: Building an environment ★
;-------------------------------------------------------------------------------
#|
(build-env bindings) -> (hash/c symbol? integer?)
  bindings: datum?
    A sequence of name bindings (so think of this as a list of individual
    "binding" datums). Follows the grammar from the exercise handout.

    You may also assume that all names are defined before they are referenced,
    and no names are bound more than once (so `bindings` is semantically-valid).

  Returns a hash table representing the environment constructed from this
  sequence of bindings (mapping symbols representing identifiers to their
  corresponding integer values).

  Hash table reference: https://docs.racket-lang.org/reference/hashtables.html.

  Implementation hint:
    It may be very tempting to think about this iteratively ("loop through each
    binding and add it to a hash table").
    While we don't want you to use mutation, recall from lecture that this
    basic loop accumulator pattern can be translated into a call to foldl.
    In fact, it's possible to implement this function as just a single
    call to foldl, with an appropriate helper function!
|#


(define (helper lst table)
  (let ([key (second lst)]
        [value (third lst)])
    (if (hash-has-key? table value)
        (hash-set table key (hash-ref table value))
        (hash-set table key value))
    )
  )

(define (build-env bindings)
  (foldl helper (make-immutable-hash) bindings)
  )



(module+ test
  (test-equal? "One binding"
               (build-env (list '(define x 3)))
               (hash 'x 3))
  (test-equal? "Two bindings"
               (build-env (list '(define x 4)
                                '(define y x)))
               (hash 'x 4
                     'y 4)))


;-------------------------------------------------------------------------------
; ★ Task 2: Currying ★
;-------------------------------------------------------------------------------
; Racket provides a built-in function "curry" that does the first task for you.
; You may NOT use this function in your code---submissions which do so will
; receive a grade of ZERO on the whole exercise!
(define (curry . rest) (error "curry: You may not use this function."))

#|
(curry-2 f) -> (-> any/c (-> any/c any/c))
  f: (-> any/c any/c any/c)
    The notation is a bit tricky. Remember that the return type goes at the end,
    so this means that `f` is a *binary* function that takes two values of any type
    and returns a value of any type.

  Returns a unary function g that takes an argument x, and returns a
  new unary function h that takes an argument y, such that
  (h y) is equivalent to (f x y).

  Here is a simple Python-style doctest for curry-2.
  We strongly recommend turning it into an executable test case,
  using the "module+ test" approach you've previously seen.

  > (define (add-2-mult x y) (* (+ 2 x) y))
  > (define curried (curry-2 add-2-mult))
  > ((curried 4) 5)
  30
|#
(define (curry-2 f)
  (lambda (x)
    (lambda(y)
  (f x y))))


(module+ test
  (test-equal? "curry-2/Addition"
               (let ([f (curry-2 (lambda (x y) (+ x y)))])
                 ((f 3) 5))  ; (f 3) is a function call!
               8))


#|
(define fix-first x f) -> (-> any/c ... any/c)
  x: any/c
  f: (-> any/c any/c ... any/c)
    f is a function that takes at least 1 argument (the middle "any/c ..."
    means that it can take an arbitrary number of arguments after the first one.

  Returns a new function g that takes one fewer argument than f, such that

      (g x2 x3 ... xn) == (f x x2 x3 ... xn)

  Hint: because the arity (number of arguments) of the returned function
  depends on the input f, you can't use something like (lambda (y z) ...) to
  define it. Instead, look up "Racket rest arguments" and the function `apply`
  to implement fix-first.

  Note: you don't need to do any "typechecking"---if the user passes the wrong
  number of arguments to the returned function, calling f should raise an error.
|#
(define (fix-first x f)
  (lambda y
    (apply f x y))
  )





(module+ test
  (test-equal? "fix-first/ternary"
              (let ([f2 (fix-first 3
                                   (lambda (x y z) (+ x (* y z))))])
                (f2 5 8))
              ; We've deliberately left the body unexpanded to show what's going on.
              (+ 3 (* 5 8))))


(module+ test
  (test-equal? "fix-first/qernary"
              (let ([f2 (fix-first 4
                                   (lambda (x y z g) (+ x (+ y (+ z g)))))])
                (f2 5 8 2))
              ; We've deliberately left the body unexpanded to show what's going on.
              (+ 4 (+ 5 (+ 8 2)))))

(module+ test
  (test-equal? "fix-first/qernary"
              (let ([f2 (fix-first 4
                                   (lambda (x y) (+ x y)))])
                (f2 5))
              ; We've deliberately left the body unexpanded to show what's going on.
              (+ 4 5)))


#|
(curry-n n f) -> procedure?
  n: (and/c integer? positive?)
    (This means n >= 1.)
  f: procedure?
    Precondition: f takes exactly n arguments.

  A generalization of curry-2, except now f takes n arguments;
  curry-n returns a function g that is the curried version of f.
  (We coudln't express this exactly in the return type, which is why we just
  wrote `procedure?`.)

  Notes and hints:
    1. It is possible to define curry-2 in terms of curry-n
       (but doing curry-2 yourself first is easier).
    2. Review the handout's examples of currying to determine the correct
       *recursive structure* for this function.
    3. If g = (curry-n n f), how is (g x) related to (fix-first x f)?
|#
(define (curry-n n f)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) (curry-n (- n 1) (fix-first x f)))
  )
)




; NOTE: don't uncomment the test below until you've written
; your own test for a good *base case* for curry-n!
; (Until you're confident in your base case, you likely won't
; have much luck with a correct recursive case.)
(module+ test

  (test-equal? "curry-n/ternary"
               (let ([f3 (curry-n 3
                                  (lambda (x y z) (+ x (* y z))))])
                 (((f3 3) 5) 8))
               (+ 3 (* 5 8)))
 (test-equal? "curry-n/bernary"
               (let ([f3 (curry-n 2
                                  (lambda (x y) (+ x y)))])
                 ((f3 3) 5))
               (+ 3 5))
  (test-equal? "curry-n/bernary"
               (let ([f3 (curry-n 1
                                  (lambda (x) (max x)))])
                 (f3 3))
               (max 3)))