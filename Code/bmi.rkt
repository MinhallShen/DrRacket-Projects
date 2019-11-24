;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bmi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 1 Question 3
;; ***************************************************
;;

;; (body-mass-index m h) calculates someone's body
;; mass index given their mass and height such that
;; m and h > 0 in kilograms and metres, respectively.
;; body-mass-index: Num Num -> Num
;; example:
(check-within (body-mass-index 55 1.73) 18.3768 0.01)

(define (body-mass-index m h)
  (/ m
     (sqr h)))

;; tests:
(check-within (body-mass-index 60 1.80) 18.518 0.01)
(check-within (body-mass-index 90 1.55) 37.4609 0.01)

;; (body-mass-index-imperial lbs ft in) calculates
;; someone's BMI given their mass and height in
;; pounds and feet+inches, respectively. 0 < in < 12
;; body-mass-index-imperial: Num Nat Num -> Num
;; example:
(check-within (body-mass-index-imperial 125 5 8) 19.005 0.01)

(define (body-mass-index-imperial lbs ft in)
  (body-mass-index (* lbs 0.45359237)
                   (* (+ (* ft 12)
                         in)
                      0.0254)))

;; tests:
(check-within (body-mass-index-imperial 170 6 3) 21.248 0.01)
(check-within (body-mass-index-imperial 120.8 5 5.7) 19.676 0.01)



