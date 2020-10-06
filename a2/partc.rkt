#lang racket

(require racket/control)

(provide -< next! backtrack! all-orderings all-perfect-reorderings sudoku4)


; This is the implementation of -< by the end of Lecture 14.

(define choices (box '()))
(define (add-choice! val) (set-box! choices (cons val (unbox choices))))
(define (get-choice!) (let* ([unboxed (unbox choices)]
                             [val (car unboxed)])
                        (begin (set-box! choices (cdr unboxed))
                               val)))

(define-syntax -<
  (syntax-rules ()
    [(-< expr) ; "when there's only candidate, there's only one choice"
     expr]

    ; Multiple choices: return the first one and store the amb
    ; that produces all the others in choices.
    [(-< expr1 expr2 ...)
     (shift k
            (begin (add-choice! (thunk (k (-< expr2 ...))))
                   (k expr1)))]))

(define (next!)
  (if (empty? (unbox choices))
      (shift k 'done)
      (reset ((get-choice!)))))

(define (backtrack!)
  (shift k (next!)))

(define (?- pred expr)
  (if (pred expr)
      expr
      (backtrack!)))

;; Question 9: Reorderings and perfect reorderings of a list

(define (all-orderings l)
  #f)

(define (all-perfect-reorderings l)
  #f)

; Question 10: sudoku4

;Your task is to implement the function sudoku-4, which takes a partial board,
;and returns a complete board which is consistent with the partial board, and
;is valid according to the above rules. You should use backtracking, and so
;calling next after calling sudoku-4 should yield different valid solutions to
;the initial partial board.

(define (sudoku4 grid)
  #f)
