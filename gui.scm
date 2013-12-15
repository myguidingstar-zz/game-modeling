#lang racket

(require racket/gui/base
         "core.scm")

(define cell-size 20)

(define frame (new frame% [label "Game modeling"]
                   [width 300]
                   [height 300]))
; Make the drawing area
(define canvas (new canvas% [parent frame]))
; Get the canvas's drawing context
(define dc (send canvas get-dc))

(define (draw-cell color x y)
  (send dc set-brush color 'solid)
  (send dc set-pen "white" 1 'solid)
  (send dc draw-rectangle x y cell-size cell-size))

(define (draw-world world)
  (let ([n-of-rows (length world)]
        [n-of-cols (vector-length (first world))])
    (for* ([row (in-range n-of-rows)]
           [col (in-range n-of-cols)])
          (draw-cell (symbol->color
                      (coor->color world (list col row)))
                     (* cell-size col)
                     (* cell-size row)))))

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
           (display (format "changed ~a times" n))
           (draw-world world)
           (sleep delay)
           (change-world-once! world)))

(define initial-world
  (create-world 400 400 300 300))

(new button% [parent frame]
     [label "Next 365 days"]
     ;; Callback for a button click:
     [callback (lambda (button event)
                 (change-world! initial-world 365 1))])

(send frame show #t)
(sleep/yield 1)
