;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sequences) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 9 Question 3
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; (sequence num1 num2 sym) consumes the first two numbers of a potential
;;   and a symbol, 'arithmetic or 'geometric, and produces a function that
;;   consumes a number representing the length of the list and produces a list
;;   containing the numbers in the arithmetic or geometric sequence, depending
;;   the symbol.
;; sequence: Num Num Sym -> (Nat -> (listof Num))
;; Requires: sym must be (oneof 'arithmetic 'geometric)
;; examples:
(check-expect (f 6) '(5 7 9 11 13 15))
(check-expect (g 4) '(2 6 18 54))

(define (sequence num1 num2 sym)
  (local
    [(define (arithmetic-seq len)
       (map (lambda (i) (+ (* i (- num2 num1)) num1))
            (build-list len (lambda (j) j))))
     (define (geometric-seq len)
       (map (lambda (i) (* num1 (expt (/ num2 num1) i)))
            (build-list len (lambda (j) j))))]
    (cond
      [(symbol=? sym 'arithmetic) arithmetic-seq]
      [else geometric-seq])))

;; constants:
(define f (sequence 5 7 'arithmetic))
(define g (sequence 2 6 'geometric))

;; tests:
(check-expect (f 6) '(5 7 9 11 13 15))
(check-expect (g 4) '(2 6 18 54))
(check-expect (f 0) empty)
(check-expect (g 0) empty)
(check-expect (f 1) '(5))
(check-expect (g 1) '(2))



