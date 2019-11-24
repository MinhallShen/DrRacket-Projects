;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus-a01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 1 Bonus Question
;; ***************************************************
;;

;; (cs-135-participation num-of-q right wrong) caculates
;; a student's CS135 participation mark given the
;; number of questions, the number of right questions
;; and the number of wrong questions such that  num-of-q,
;; right, wrong >= 0
;; cs135-participation: Nat Nat Nat -> Num
;; example:
(check-expect (cs135-participation 20 12 4) 90)

(define (cs135-participation num-of-q right wrong)
        (* (/ (+ (* 2 (min (* 0.75 num-of-q)
                           right))
                 (min (- (* 0.75 num-of-q)
                         (min (* 0.75 num-of-q) right))
                      wrong))
              (* 0.75 num-of-q 2))
           100))

;; tests:
(check-expect (cs135-participation 20 16 2) 100)
(check-expect (cs135-participation 20 16 0) 100)
(check-expect (cs135-participation 40 20 5) 75)
(check-expect (cs135-participation 20 10 4) 80)
