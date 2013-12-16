#lang racket

(require racket/gui/base
         "core.rkt")

;; Configuration goes here
(define cell-size 10)

(define time-between-turns 0.0001)

(define n-of-steps-each-click (* 365 1000000))

(define n-of-Os 250)
(define n-of-Xs 250)
(define n-of-cols 30)
(define n-of-rows 30)

;; Main program
(define frame (new frame% [label "Game modeling"]
                   [width (* cell-size n-of-cols)]
                   [height (* cell-size n-of-rows)]))
; Make the drawing area
(define canvas (new canvas% [parent frame]))
; Get the canvas's drawing context
(define dc (send canvas get-dc))

(define (draw-cell color x y)
  (send dc set-brush color 'solid)
  (send dc set-pen "white" 1 'solid)
  (send dc draw-rectangle (* cell-size x) (* cell-size y) cell-size cell-size))

(define (draw-world world)
  (let ([n-of-rows (length world)]
        [n-of-cols (vector-length (first world))])
    (for* ([row (in-range n-of-rows)]
           [col (in-range n-of-cols)])
          (draw-cell (symbol->color
                      (coor->color world (list col row)))
                     col
                     row))))

(define (change-world-once! world)
  (let ([pair (decide-cells-to-swap* world)])
    (if (void? pair)
        #f
        (let* ([source (first pair)]
               [target (second pair)])
          (swap-cells! world source target)
          #t))))

(define (change-world! world n-of-steps delay)
  (for/and ([n n-of-steps])
           (print (format "changed ~a times" n))
           (draw-world world)
           (sleep delay)
           (change-world-once! world)))

(define initial-world
  (create-world n-of-Os n-of-Xs
                n-of-cols n-of-rows))

(new button% [parent frame]
     [label (format "Next ~a turns" n-of-steps-each-click)]
     ;; Callback for a button click:
     [callback (lambda (button event)
                 (change-world! initial-world
                                n-of-steps-each-click time-between-turns))])

(send frame show #t)
(sleep/yield 1)
