;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 1 Question 4
;; ***************************************************
;;

;; (final-cs135-grade m1 m2 fin assn) calculates a
;; CS135 student's final grade given midterm 1 m1,
;; midterm 2 m2, final exam fin and assignments
;; assn.
;; final-cs135-grade: Num Num Num Num -> Num
;; example:
(check-expect (final-cs135-grade 60 70 64 90) 71.5)

(define (final-cs135-grade m1 m2 fin assn)
        (+ (* 0.1 m1)
           (* 0.15 m2)
           (* 0.5 fin)
           (* 0.2 assn)
           5))
;; tests:
(check-expect (final-cs135-grade 0 0 0 0) 5)
(check-expect (final-cs135-grade 50.4 30.68 98.8 46.7) 73.382)

;; (cs135-final-exam-grade-needed m1 m2 assn) calculates
;; the mark a student needs on his/her final exam in
;; order to pass the CS135 course.
;; cs135-final-exam-grade-needed: Num Num Num -> Num
;; example:
(check-expect (cs135-final-exam-grade-needed 80 90 70) 39)

(define (cs135-final-exam-grade-needed m1 m2 assn)
        (* 2
           (- 60
              (+ (* 0.1 m1)
                 (* 0.15 m2)
                 (* 0.2 assn)
                 5))))

;; tests:
(check-expect (cs135-final-exam-grade-needed 0 0 0) 110)
(check-expect (cs135-final-exam-grade-needed 100 100 100) 20)
(check-expect (cs135-final-exam-grade-needed 50.6 30.495 68.1234) 63.48214) 