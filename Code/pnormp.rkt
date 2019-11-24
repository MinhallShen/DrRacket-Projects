;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pnormp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 3 Question 2
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; (pnormp p v) calculates the p-norm to the exponent p of a given list of
;;   numbers and an exponent p.
;; pnormp: Nat (listof Num) --> Num 
;; Requires: p != 0 and (length v) <= 3
;; examples:
(check-expect (pnormp 3 (cons 3 (cons -4 (cons 5 empty)))) 216)
(check-expect (pnormp 4 (cons 2 (cons -3 empty))) 97)
(check-expect (pnormp 1 (cons -2 empty)) 2)
              
(define (pnormp p v)
  (cond
    [(empty? v) 0]
    [(empty? (rest v))
     (expt (abs (first v)) p)]
    [(empty? (rest (rest v)))
     (+ (expt (abs (first v)) p)
        (expt (abs (first (rest v))) p))]
    [else
     (+ (expt (abs (first v)) p)
        (expt (abs (first (rest v))) p)
        (expt (abs (first (rest (rest v)))) p))]))

;; tests:
(check-expect (pnormp 3 empty) 0)
(check-expect (pnormp 1 (cons 0 empty)) 0)
(check-expect (pnormp 1 (cons -2 empty)) 2)
(check-expect (pnormp 1 (cons 3 empty)) 3)
(check-expect (pnormp 3 (cons -2 (cons -3 empty))) 35)
(check-expect (pnormp 2 (cons -2 (cons 2 empty))) 8)
(check-expect (pnormp 2 (cons 2 (cons -2 empty))) 8)
(check-expect (pnormp 4 (cons 2 (cons 4 empty))) 272)
(check-expect (pnormp 3 (cons -3 (cons -4 (cons -5 empty)))) 216)
(check-expect (pnormp 3 (cons 3 (cons -4 (cons -5 empty)))) 216)
(check-expect (pnormp 3 (cons -3 (cons -4 (cons 5 empty)))) 216)
(check-expect (pnormp 3 (cons -3 (cons 4 (cons 5 empty)))) 216)
(check-expect (pnormp 3 (cons 3 (cons -4 (cons 5 empty)))) 216)
(check-expect (pnormp 3 (cons 3 (cons 4 (cons -5 empty)))) 216)
(check-expect (pnormp 3 (cons 3 (cons 4 (cons 5 empty)))) 216)



                      