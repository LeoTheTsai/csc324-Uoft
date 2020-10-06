#lang racket
#|
Module: ex5
Description: Exercise 5: Extending the my-class Macro
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Summer 2020

|#
;-------------------------------------------------------------------------------
(provide my-class-getter my-class-setter)

; Our my-class macro from lecture:

;; Task 1

(define-syntax my-class-getter
  (syntax-rules (method)
    ; The template the macro will match against...
    [(my-class <cname> (<field> ...)
               (method <mname> <margs> ... <mbody>)...)
     (begin
     ; Define the constructor function with the name of the class
       (define (<cname> <field> ...)
         (letrec ([__class__ (make-immutable-hash
                              (list
                               (cons (quote <mname>) (λ (<margs> ...) <mbody>)) ...))]
                  [__dict__ (make-immutable-hash
                             (list
                              (cons (quote <field>) <field>) ...))]
                  [self
                   ; Define the "object function" that processes received messages
                   (λ (msg)
                     (cond [(hash-has-key? __dict__ msg)
                            (hash-ref __dict__ msg)]
                           [(hash-has-key? __class__ msg)
                            (λ args (apply (hash-ref __class__ msg) (cons self args)))]
                           (error (format "Unknown msg ~a in ~a" msg (quote <cname>)))))])
           self))
     
       ; Other definitions go here..
       (define (<field> <cname>)
         (<cname> (quote <field>))
         ) ...)
       ]))

(module+ test
  (require rackunit)

  ; We use `local` to create an isolated "top-level" scope for our definitions.
  ; https://docs.racket-lang.org/reference/local.html
  ;
  ; Run these tests when you're ready!
  (local
      [(my-class-getter Point (x y)
         (method size self
                 (sqrt (+ (* (self 'x) (self 'x)) (* (self 'y) (self 'y)))))

         (method scale self n
                 (Point (* (self 'x) n) (* (self 'y) n))))]
      (test-true "x and y are functions" (and (procedure? x) (procedure? y)))
      (test-equal? "x and y are accessors"
                   (let* ([p (Point 2 3)])
                     (list (x p) (y p)))
                   (list 2 3))))


;; Task 2

(define-syntax my-class-setter
  (syntax-rules (method)
    ; The template the macro will match against...
    [(my-class <cname> (<field> ...)
               (method <mname> <margs> ... <mbody>)...)

     ; Define the constructor function with the name of the class
     (define (<cname> <field> ...)
       (letrec ([__class__ (make-immutable-hash
                            (list
                             (cons (quote <mname>) (λ (<margs> ...) <mbody>)) ...))]
                [__dict__ (make-immutable-hash
                           (list
                            (cons (quote <field>) <field>) ...))]
                [self
                 ; Define the "object function" that processes received messages
                 (create-object <cname> __class__ __dict__)
                 ])
         self))]))


; You may find it helpful to factoring out creating the "object function", that receives messages
; from the outside world, into the function below helpful.

; symbol -> hash -> hash -> (symbol -> value)
(define (create-object class-name class__dict__ self__dict__)
  (letrec ([self
            (λ (msg)
              (cond
                 [(equal? '__setattr__ msg) (lambda (id value) (create-object class-name class__dict__ (hash-set self__dict__ id value)))]
                 [(hash-has-key? self__dict__ msg)
                  (hash-ref self__dict__ msg)]
                 [(hash-has-key? class__dict__ msg)
                  (λ args (apply (hash-ref class__dict__ msg) (cons self args)))]
                 [error (format "Unknown msg ~a in ~a" msg class-name)]))])
    self))


(module+ test
  (local
    [(my-class-setter Point (x y)
       (method size self
               (sqrt (+ (* (self 'x) (self 'x))
                        (* (self 'y) (self 'y)))))

       (method scale self n
               (Point (* (self 'x) n) (* (self 'y) n))))]
    (test-true "__setattr__ is a method" (procedure? ((Point 2 3) '__setattr__)))
    (test-equal? "__setattr__ changes an attribute"
                 (let* ([p (Point 2 3)]
                        [p2 ((p '__setattr__) 'x 5)])
                   (p2 'x))
                 5)
    (test-equal? "__setattr__ adds a new attribute"
                 (let* ([p (Point 2 3)]
                        [p2 ((p '__setattr__) 'z 5)])
                   (p2 'z))
                 5)
    (test-equal? "__setattr__ doesn't mutate original object"
                 (let* ([p (Point 2 3)]
                        [p2 ((p '__setattr__) 'x 5)])
                   (p 'x))
                 2)
    (test-equal? "Multiple __setattr__ chained"
                 (let* ([p (Point 2 3)]
                        [p2 ((p '__setattr__) 'x 5)]
                        [p3 ((p2 '__setattr__) 'x 10)])
                   (p3 'x))
                 10)))

