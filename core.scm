#lang racket

;; faster naming so that sample input can be typed with ease
;; These names' values are symbols (instead of color strings)
;; so that they can be printed with same look.

(define O 'O)
(define X 'X)
(define _ '_)

;; creates an initial state for a new world
(define (create-world n-of-Os n-of-Xs n-of-cols n-of-rows)
  "todo")

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
(define (coor->color world cell-coor)
  (let ([col (first cell-coor)]
        [row (second cell-coor)])
    (vector-ref (list-ref world row) col)))

;; set color for a specific cell
(define (set-color! world cell-coor color)
  (let ([col (first cell-coor)]
        [row (second cell-coor)])
    (vector-set! (list-ref world row) col color)))

;; swaps two cells (swap their colours)
(define (swap-cells! world source target)
  (let ([source-color (coor->color world source)]
        [target-color (coor->color world target)])
    (set-color! world  source target-color)
    (set-color! world  target source-color)))

;; returns list of all cells nearby the given cell with a specific distance
(define (find-nearby-cells world central-cell distance)
  "todo")

;; returns list of all free spaces (cells) nearby the given cell with a specific distance
(define (find-free-spaces world central-cell distance)
  "todo")

;; returns list of colors of the given cell's neighbors
(define (get-neighbors-colors world central-cell)
  "todo")

(define (colors->minority-index neighbors-colors cell-color)
  (let* ([non-empty-neighbors
         (filter-not (lambda [x] (equal? _ x))
                     neighbors-colors)]
        [same-color-neighbors
         (filter (lambda [x] (equal? cell-color x))
                 neighbors-colors)]
        [total (add1 (length non-empty-neighbors))])
    (/ (add1 (length same-color-neighbors)) total)))
;; receives a cell's color and its neighbors' colors
;; returns true if the cell is minor
(define (minor? neighbors-colors cell-color)
  (< (colors->minority-index neighbors-colors cell-color)
     0.4))

;; exports symbols into a module that can be reused from other files
(provide O X _
         coor->color
         symbol->color
         set-color!
         swap-cells!
         get-neighbors-colors
         colors->minority-index
         minor?
         find-nearby-cells
         find-free-spaces)
