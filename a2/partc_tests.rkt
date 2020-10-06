#lang racket

(require "partc.rkt")
(require rackunit)
(require racket/control)

; Consumes a ambiguous expression and flattens all generated choice
; points into a list.  Notice our use of `reset` in order to delimit the
; continuation produced inside (next!) and the supplied -<.
(define-syntax amb->list
  (syntax-rules ()
    [(amb->list amb)
     (letrec ([initial (reset amb)]
              [iter (λ (l)
                      (let ([curr (reset (next!))])
                        (if (equal? curr 'done)
                            l
                            (iter (cons curr l)))))])
       (iter (list initial)))]))


#;(module+ test
    ; We convert the list to a set in order to disregard ordering
    ; information; the order in which you produce the result is
    ; up to you.
    (check-equal?
     (list->set (amb->list (all-orderings '(1 2 3))))
     (list->set '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))))

    (check-equal?
     (list->set (amb->list (all-perfect-reorderings '(1 2 3))))
     (list->set '((3 1 2) (2 3 1)))))
     

#;(module+ test
  (check-equal? (sudoku4 '((4  1  3  2 )
                           (2  3  "" 4 )
                           (1  2  4  3 )
                           (3  4  2  1)))
                '((4  1  3  2 )
                  (2  3  1  4 )
                  (1  2  4  3 )
                  (3  4  2  1)))
  
  (check-equal? (sudoku4 '((1  2  3  4 )
                           (4  3  "" "")
                           (3  4  2  1 )
                           (2  4  1  3)))
                '((1  2  3  4 )
                  (4  3  1  2)
                  (3  4  2  1 )
                  (2  4  1  3)))

  (check-equal? (sudoku4 '((4  2  1  3 )
                           (1  "" "" 4 )
                           ("" 1  "" 2 )
                           (2  4  3  1)))
                '((4  2  1  3 )
                  (1  3  2  4 )
                  (3  1  4  2 )
                  (2  4  3  1))))

