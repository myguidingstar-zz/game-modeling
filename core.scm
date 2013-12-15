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
;; the nearby cells may or may not be inside world.
(define (find-nearby-cells-helper central-cell distance)
  (let* ([col (first central-cell)]
         [row (second central-cell)]

         [top (- row distance)]
         [bottom (+ row distance)]
         [left (- col distance)]
         [right (+ col distance)])
    (append
     (for/list ([i (in-range left (add1 right))])
               (list i top))
     (for/list ([i (in-range  (add1 top) bottom)])
               (list right  i))
     (for/list ([i (in-range left (add1 right))])
               (list i bottom))
     (for/list ([i (in-range  (add1 top) bottom)])
               (list left  i)))))

;; returns list of all cells nearby the given cell with a specific distance
;; nearby cells must be inside world.
(define (find-nearby-cells world central-cell distance)
  (let ([all-nearby-cells
         (find-nearby-cells-helper central-cell distance)]
        [world-right (vector-length (first world))]
        [world-bottom (length world)])
    (filter (lambda [cell-coor]
              (let ([col (first cell-coor)]
                    [row (second cell-coor)])
                (and (< -1 col world-right)
                     (< -1 row world-bottom))))
            all-nearby-cells)))

;; returns list of all free spaces (cells) nearby the given cell with a specific distance
(define (find-free-spaces world central-cell distance)
  (let ([nearby-cells
         (find-nearby-cells world central-cell distance)])
    (filter (lambda [cell-coor]
              (equal? _ (coor->color world cell-coor)))
            nearby-cells)))

;; returns list of colors of the given cell's neighbors
(define (get-neighbors-colors world central-cell)
  (let ([neighbors (find-nearby-cells world central-cell 1)])
    (map (lambda (cell-coor)
           (coor->color world cell-coor))
         neighbors)))

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

(define (all-cells world)
  (for*/list
   ([row (in-range (length world))]
    [col (in-range (vector-length (first world)))])
   (list col row)))

(define (most-minor-agents world)
  (let* ([cells (all-cells world)]
         [minority-indexes
          (for/list ([coor (in-list cells)]
                     #:when (not (equal? _ (coor->color world coor))))
                    (list
                     coor
                     (colors->minority-index
                      (get-neighbors-colors world coor)
                      (coor->color world coor))))]
         [lowest-minority-index
          (apply min (map second minority-indexes))])
    (if (>= 0.4 lowest-minority-index)
     (map first
          (filter (lambda [entry]
                    (let ([coor (first entry)]
                          [minority-index (second entry)])
                      (= lowest-minority-index
                         minority-index)))
                  minority-indexes))
     '())))

;; finds free spaces with the same distance to the given central cell
;; - starts finding with distance = 1
;; - will increase distance if no free space found inside current distance
;; - distance is limited within a number
;; - returns #f if no free space found inside limit distance
(define (find-free-spaces-incrementally
         world central-cell limit-distance)
  (for/first ([i (in-range 1 (add1 limit-distance))]
              #:when (< 0 (length
                           (find-free-spaces
                            world central-cell i))))
             (find-free-spaces
              world central-cell i)))
;; exports symbols into a module that can be reused from other files
(provide O X _
         coor->color
         symbol->color
         set-color!
         swap-cells!
         get-neighbors-colors
         colors->minority-index
         all-cells
         minor?
         most-minor-agents
         find-nearby-cells-helper
         find-nearby-cells
         find-free-spaces
         find-free-spaces-incrementally)
