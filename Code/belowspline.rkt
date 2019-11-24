;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname belowspline) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 3 Question 4
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; a)
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
(define origin (make-point 0 0))
(define end-of-spline (make-point 10 0))
;; (find-slope p1 p1) calculates the slope of a line given two lists each
;;   containing a pair of points.
;; find-slope: (listof Num) (listof Num) -> Num
;; example:
(check-expect (find-slope (cons 0 (cons 0 empty))
                          (cons 1 (cons 2 empty))) 2)

(define (find-slope p1 p2)
  (/ (- (y-coord p1) (y-coord p2))
     (- (x-coord p1) (x-coord p2))))

;; (y-int slope point) determines the y-intercept of a line given two points
;;   on the line
;; y-int: (listof Num) (listof Num) -> Num
;; examples:
(check-expect (y-int (make-point 1 1) (make-point 2 2)) 0)
(check-expect (y-int (make-point 4 3) (make-point 2 2)) 1)

(define (y-int p1 p2)
  (- (y-coord p1) (* (find-slope p1 p2) (x-coord p1))))

;; (below-spline? P1 P2 ab) determines whether or not a point is within the
;;   shaded area.
;; below-spline?: (listof Num) (listof Num) (listof Num) -> Bool
;; requires: the x-coordinates of P1 must be less than the x-coordinates of P2
;;   and greater than 0, the y-coordinates of P1 must be less than the
;;   y-coordinates of P2, and the x-coordinates of P1 and P2 must be between
;;   0 and 10, exclusive.
;; example:
(check-expect (below-spline? (make-point 3 2)
                             (make-point 7 3)
                             (make-point 3 2)) true)
(check-expect (below-spline? (make-point 3 2)
                             (make-point 7 3)
                             (make-point -1 2)) false)
(check-expect (below-spline? (make-point 3 2)
                             (make-point 7 3)
                             (make-point 1 1)) false)
                  
(define (below-spline? P1 P2 ab)
  (cond
   [(and (and (> (x-coord ab) 0)
              (<= (x-coord ab) (x-coord P1)))
         (and (>= (y-coord ab) 0)
              (<= (y-coord ab) (+ (* (find-slope origin P1)
                                     (x-coord ab))
                                  (y-int origin P1)))))
    true]
   [(and (and (> (x-coord ab) (x-coord P1))
              (<= (x-coord ab) (x-coord P2)))
         (and (>= (y-coord ab) 0)
              (<= (y-coord ab) (y-coord P1))))
    true]
   [(and (and (> (x-coord ab) (x-coord P1))
              (<= (x-coord ab) (x-coord P2)))
         (and (> (y-coord ab) (y-coord P1))
              (<= (y-coord ab) (+ (* (find-slope P1 P2)
                                    (x-coord ab))
                                 (y-int P1 P2)))))
    true]
   [(and (and (> (x-coord ab) (x-coord P2))
              (< (x-coord ab) 10))
         (and (>= (y-coord ab) 0)
              (<= (y-coord ab) (+ (* (find-slope P2 end-of-spline)
                                     (x-coord ab))
                                     (y-int P2 end-of-spline)))))
    true]
   [else false]))
                                    
              
                    

(define P1 (make-point 3 2))
(define P2 (make-point 7 3))
(check-expect (below-spline? P1 P2 (make-point 3 2)) true)
(check-expect (below-spline? P1 P2 (make-point 7 3)) true)
(check-expect (below-spline? P1 P2 (make-point 1 2/3)) true)
(check-expect (below-spline? P1 P2 (make-point 5 5/2)) true)
(check-expect (below-spline? P1 P2 (make-point 17/2 3/2)) true)
(check-expect (below-spline? P1 P2 (make-point 1 1/2)) true)
(check-expect (below-spline? P1 P2 (make-point 5 1)) true)
(check-expect (below-spline? P1 P2 (make-point 5 12/5)) true)
(check-expect (below-spline? P1 P2 (make-point 9 1/2)) true)
(check-expect (below-spline? P1 P2 (make-point 1 0)) true)
(check-expect (below-spline? P1 P2 (make-point 5 0)) true)
(check-expect (below-spline? P1 P2 (make-point 9 0)) true)
(check-expect (below-spline? P1 P2 (make-point 5 (+ 5/2 1/100))) false)




               


          
               
               


          
     
     
  


  