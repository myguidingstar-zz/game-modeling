#lang racket
(require rackunit
         "core.scm"
         rackunit/text-ui)

(define core-tests
  (test-suite
   "Tests for core.scm"
   (check-equal?
    (coor->color
     (list (vector X X)
           (vector O _))
     '(0 1))
    O)
   (check-equal?
    (let ([world (list
                  (vector O O O _)
                  (vector _ X O X)
                  (vector O O _ _)
                  (vector O X O _))])
      (set-color! world '(2 1) X)
      world)
    (list
     (vector O O O _)
     (vector _ X X X)
     (vector O O _ _)
     (vector O X O _))
    "set-color! test")
   (check-equal?
    (let ([world (list
                  (vector O O O _)
                  (vector _ X O X)
                  (vector O O _ _)
                  (vector O X O _))])
      (swap-cells! world '(1 1) '(0 1))
      world)

    ;; Input world with highlighted source and target cells:
    ;; - source cell is marked with brackets []
    ;; - target cell is marked with curly    {}
    ;; (list
    ;;  (vector  O   O  O _)
    ;;  (vector {_} [X] O X)
    ;;  (vector  O   O  _ _)
    ;;  (vector  O   X  O _))

    ;; Output world
    (list
     (vector O O O _)
     (vector X _ O X)
     (vector O O _ _)
     (vector O X O _))
    "swap-cells! tests")
   (check-equal?
    (colors->minority-index '(O O O O _ O O _) X)
    1/7
    "colors->minority-index test")
   (check-equal?
    (minor? '(O O O O _ O O _) X)
    #t
    "minor? return #t test")
   (check-equal?
    (minor? '(O O O O _ O O _) O)
    #f
    "minor? return #f test")
   (check-equal?
    (get-neighbors-colors
     (list
      (vector O O O _)
      (vector _ X O X)
      (vector O O _ _)
      (vector O X O _))
     '(1 1))
    ;; order: NW - N - NE - E - SE - S - SW - W
    '(O O O O _ O O _)
    "")
   (check-equal?
    (find-free-spaces
     (list
      (vector O O O _)
      (vector _ X O X)
      (vector O O _ _)
      (vector O X O _))
     '(1 1)
     1)
    ;; Central cell is marked with brackets []
    ;; Free spaces with distance 1 are marked with curly {}
    ;; (list
    ;;  (vector  O   O   O  _)
    ;;  (vector {_} [X]  O  X)
    ;;  (vector  O   O  {_} _)
    ;;  (vector  O   X   O  _))
    ;; order: NW - N - NE - E - SE - S - SW - W
    '((2 2) (0 1)))
   (check-equal?
    (find-nearby-cells
     (list
      (vector O O O _)
      (vector _ X O X)
      (vector O O _ _)
      (vector O X O _))
     '(1 1)
     2)
    '((3 0) (3 2) (3 3))
    "find-free-spaces with distance 1 test")

   (check-equal?
    (find-nearby-cells-helper '(1 1) 1)
    '((0 0) (1 0) (2 0) ;; upper row (including upper corners)
      (2 1)             ;; right col (excluding right corners)
      (0 2) (1 2) (2 2) ;; lower row (including lower corners)
      (0 1))            ;; right col (excluding right corners)
    "find-nearby-cells-helper with distance 1 test")

   (check-equal?
    (find-nearby-cells-helper '(1 1) 2)
    '((-1 -1) (0 -1) (1 -1) (2 -1) (3 -1)
      (3   0) (3  1) (3  2)
      (-1  3) (0  3) (1  3) (2  3) (3  3)
      (-1  0) (-1 1) (-1 2))
    "find-nearby-cells-helper with distance 2 test")

   (check-equal?
    (find-nearby-cells
     (list
      (vector O O O _)
      (vector _ X O X)
      (vector O O _ _)
      (vector O X O _))
     '(1 1)
     1)
    ;; Central cell is marked with brackets []
    ;; nearby cells with distance 1 are marked with curly {}
    ;; (list
    ;;  (vector {O} {O} {O} _)
    ;;  (vector {_} [X] {O} X)
    ;;  (vector {O} {O} {_} _)
    ;;  (vector  O   X   O  _))
    '((0 0) (1 0) (2 0) ;; upper row (including upper corners)
      (2 1)             ;; right col (excluding right corners)
      (0 2) (1 2) (2 2) ;; lower row (including lower corners)
      (0 1))            ;; right col (excluding right corners)
    "find-nearby-cells test")))

(run-tests core-tests)
