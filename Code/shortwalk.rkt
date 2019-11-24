;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname shortwalk) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 3 Question 5
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; (x-coord pts) consumes a pair of coordinates and produces the x
;;   coordinate.
;; x-coord: (listof Num) -> Num
;; example:
(check-expect (x-coord (cons 2 (cons 1 empty))) 2)

(define (x-coord lst)
  (first lst))

;; (y-coord pts) consumes a pair of coordinates and produces the y
;;   coordinate.
;; y-coord: (listof Num) -> Num
;; example:
(check-expect (y-coord (cons 2 (cons 1 empty))) 1)

(define (y-coord lst)
  (second lst))

;; (make-point x y) produces a list containing two values representing the x
;;   and y coordinates given the x and y coordinate.
;; make-point x y: Num Num -> (listof Num)
;; example:
(check-expect (make-point 1 2)
              (cons 1 (cons 2 empty)))

(define (make-point x y)
  (cons x (cons y empty)))

;; tests:
(check-expect (make-point 0 0)
              (cons 0 (cons 0 empty)))
(check-expect (make-point 2 2 )
              (cons 2 (cons 2 empty)))
(check-expect (make-point 2 -2)
              (cons 2 (cons -2 empty)))
(check-expect (make-point -2 2)
              (cons -2 (cons 2 empty)))
(check-expect (make-point -2 -2)
              (cons -2 (cons -2 empty)))
(check-expect (make-point 0 2)
              (cons 0 (cons 2 empty)))
(check-expect (make-point 0 -2)
              (cons 0 (cons -2 empty)))
(check-expect (make-point 2 0)
              (cons 2 (cons 0 empty)))
(check-expect (make-point -2 0)
              (cons -2 (cons 0 empty)))

;------------------------------------------------------------------------------

;; a)
;; (make-step start-point dir dist) calculates the coordinates of a point after
;;   moving a certain number of steps in a direction
;; make-step: (listof Num) Sym Num -> (listof Num)
;; requires: dist = 'N, 'E, 'S or 'W and dist > 0
;; examples:
(check-expect (make-step (make-point 2 3) 'E 4) (make-point 6 3))
(check-expect (make-step (make-point 5 -3) 'S 2) (make-point 5 -5))

(define (make-step start-point dir dist)
  (cond
    [(symbol=? dir 'E)
     (make-point (+ (x-coord start-point) dist) (y-coord start-point))]
    [(symbol=? dir 'W)
     (make-point (- (x-coord start-point) dist) (y-coord start-point))]
    [(symbol=? dir' N)
     (make-point (x-coord start-point) (+ (y-coord start-point) dist))]
    [(symbol=? dir' S)
     (make-point (x-coord start-point) (- (y-coord start-point) dist))]))

;; tests:
(check-expect (make-step (make-point 2 3) 'E 4) (make-point 6 3))
(check-expect (make-step (make-point 5 -3) 'S 2) (make-point 5 -5))
(check-expect (make-step (make-point 2 3) 'W 4) (make-point -2 3))
(check-expect (make-step (make-point 5 -3) 'N 2) (make-point 5 -1))
(check-expect (make-step (make-point 2 3) 'E -4) (make-point -2 3))
(check-expect (make-step (make-point 5 -3) 'S -2) (make-point 5 -1))
(check-expect (make-step (make-point 2 3) 'N -4) (make-point 2 -1))
(check-expect (make-step (make-point 5 -3) 'S -2) (make-point 5 -1))

;------------------------------------------------------------------------------

;; b)
;; (two-steps start-point directions distances) produces a set of coordinates
;;   after consuming a starting point, two directions and two distances.
;; two-steps: (listof Num) (listof Sym) (listof Num) -> (listof Num)
;; Requires: all lists have a length of two, the distances must be positive and
;;   directions must be 'N 'E 'S or 'W
;; examples:
(check-expect
(two-steps (make-point 1 -1)
(cons 'N (cons 'E empty))
(cons 2 (cons 3 empty)))
(make-point 4 1))
(check-expect
(two-steps (make-point 3 3)
(cons 'E (cons 'S empty))
(cons 4 (cons 1 empty)))
(make-point 7 2))

(define (two-steps start-point directions distances)
  (make-step (make-step start-point (first directions) (first distances))
        (second directions) (second distances)))
       
;; tests:
(check-expect (two-steps (make-point 3 3)
                         (cons 'N (cons 'N empty))
                         (cons 2 (cons 3 empty))) (make-point 3 8))
(check-expect (two-steps (make-point 3 3)
                         (cons 'N (cons 'E empty))
                         (cons 2 (cons 3 empty))) (make-point 6 5))
(check-expect (two-steps (make-point 3 3)
                         (cons 'N (cons 'S empty))
                         (cons 2 (cons 3 empty))) (make-point 3 2))
(check-expect (two-steps (make-point 3 3)
                         (cons 'N (cons 'W empty))
                         (cons 2 (cons 3 empty))) (make-point 0 5))
(check-expect (two-steps (make-point 3 3)
                         (cons 'E (cons 'N empty))
                         (cons 2 (cons 3 empty))) (make-point 5 6))
(check-expect (two-steps (make-point 3 3)
                         (cons 'E (cons 'E empty))
                         (cons 2 (cons 3 empty))) (make-point 8 3))
(check-expect (two-steps (make-point 3 3)
                         (cons 'E (cons 'S empty))
                         (cons 2 (cons 3 empty))) (make-point 5 0))
(check-expect (two-steps (make-point 3 3)
                         (cons 'E (cons 'W empty))
                         (cons 2 (cons 3 empty))) (make-point 2 3))
(check-expect (two-steps (make-point 3 3)
                         (cons 'S (cons 'N empty))
                         (cons 2 (cons 3 empty))) (make-point 3 4))
(check-expect (two-steps (make-point 3 3)
                         (cons 'S (cons 'E empty))
                         (cons 2 (cons 3 empty))) (make-point 6 1))
(check-expect (two-steps (make-point 3 3)
                         (cons 'S (cons 'S empty))
                         (cons 2 (cons 3 empty))) (make-point 3 -2))
(check-expect (two-steps (make-point 3 3)
                         (cons 'S (cons 'W empty))
                         (cons 2 (cons 3 empty))) (make-point 0 1))
(check-expect (two-steps (make-point 3 3)
                         (cons 'W (cons 'N empty))
                         (cons 2 (cons 3 empty))) (make-point 1 6))
(check-expect (two-steps (make-point 3 3)
                         (cons 'W (cons 'E empty))
                         (cons 2 (cons 3 empty))) (make-point 4 3))
(check-expect (two-steps (make-point 3 3)
                         (cons 'W (cons 'S empty))
                         (cons 2 (cons 3 empty))) (make-point 1 0))
(check-expect (two-steps (make-point 3 3)
                         (cons 'W (cons 'W empty))
                         (cons 2 (cons 3 empty))) (make-point -2 3))


