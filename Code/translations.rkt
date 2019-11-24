;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname translations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 1 Question 2
;; ***************************************************
;;


;; (volume r) produces the volume of a sphere given
;; radius r such that r >= 0
;; volume: Num -> Num
;; examples:
(check-within (volume 3) 113.097 0.01)

(define (volume r)
  (* (/ 4 3)
     pi
     (expt r 3)))

;; tests:
(check-expect (volume 0) 0)
(check-within (volume 1.547) 15.508 0.01)

;; (fib n) produces the nth value of the fibonacci
;; sequence given n such that n > 0
;; fib: Nat -> Nat
;; examples:
(check-within (fib 10) 55 0.01)

(define phi
  (/ (+ 1 (sqrt 5)) 2))

(define (fib n)
  (/ (- (expt phi n)
        (expt (* -1 phi)
              (* -1 n)))
     (- (* 2 phi) 1)))
;; tests:
(check-within (fib 1) 1 0.01)
(check-within (fib 4) 3 0.01)

;; (escape m r) produces the speed needed by a rocket to
;; escape a planet's gravity given mass m and radius r
;; escape: Num Num -> Num
;; examples:
(check-within (escape 10 10) 1.15e-05 0.01)

(define G 6.647e-11)

(define (escape m r)
  (sqrt (/ (* 2 G m) r)))

;; tests:
(check-expect (escape 0 2) 0)
(check-error (escape 0 0))
(check-within (escape 127 14950) 1.0648e-06 0.001)

;; (pressure->loudness P) calculates the loudness of
;; something in dB given pressure P, P > 0
;; pressure->loudness: Num -> Num
;; examples:
(check-within (pressure->loudness 100) 133.979 0.01)

(define (pressure->loudness P)
  (* 20 (/ (log(/ P 2e-5)) (log 10))))

;; tests:
(check-expect (pressure->loudness 2e-05) 0)
(check-within (pressure->loudness 20) 120 0.01)

