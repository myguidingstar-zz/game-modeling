#lang racket

;; faster naming so that sample input can be typed with ease
;; These names' values are symbols (instead of color strings)
;; so that they can be printed with same look.

(define O 'O)
(define X 'X)
(define _ '_)

(define (symbol->color sym)
  (cond [(equal? 'O sym)
         "red"]
        [(equal? 'X sym)
         "blue"]
        [else "white"]))

;; A sample 4x4 world should look like this
(list
 ;; cols 0 1 2 3  /  rows
 (vector O O O _) ;   0
 (vector _ X O X) ;   1
 (vector O O _ _) ;   2
 (vector O X O _));   3
