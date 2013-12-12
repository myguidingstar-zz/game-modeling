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

;; looks up cell's color by coordinates
(define (coor->color world col row)
  (vector-ref (list-ref world row) col))

;; swaps two cells (swap their colours)
(define (swap-cells! world source target)
  "todo")

;; returns list of all cells nearby the given cell with a specific distance
(define (find-nearby-cells world central-cell distance)
  "todo")

;; returns list of all free spaces (cells) nearby the given cell with a specific distance
(define (find-free-spaces world central-cell distance)
  "todo")
