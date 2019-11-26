;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname change-alf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 9 Question 2
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; constants:
(define nickel 5)
(define dime 10)
(define quarter 25)
(define loonie 100)
(define toonie 200)

;; a)
;; (count-change coin-list) calculates how much change there is in cents given
;;   a list of different Canadian coins.
;; count-change: (listof Sym) -> Nat
;; Requires: 
;; examples:
(check-expect (count-change (cons 'dime (cons 'quarter empty))) 35)
(check-expect (count-change (cons 'loonie (cons 'toonie empty))) 300)

(define (count-change coin-list)
  (foldr + 0 (map (lambda (coin) (cond
                                   [(symbol=? coin 'nickel) nickel]
                                   [(symbol=? coin 'dime) dime]
                                   [(symbol=? coin 'quarter) quarter]
                                   [(symbol=? coin 'loonie) loonie]
                                   [(symbol=? coin 'toonie) toonie]
                                   [else 0])) coin-list)))                                  

;; tests:
(check-expect (count-change (cons 'nickel
                                  (cons 'dime
                                        (cons 'quarter
                                              (cons 'loonie
                                                    (cons 'toonie empty))))))
              340)
(check-expect (count-change (cons 'nickel (cons 'not-a-coin empty))) 5)
(check-expect (count-change empty) 0)
(check-expect (count-change (cons 'not-a-coin (cons 'not-a-coin empty))) 0)
(check-expect (count-change (cons 'dime (cons 'quarter empty))) 35)
(check-expect (count-change (cons 'loonie (cons 'toonie empty))) 300)

;; b)
;; (make-change cents) produces a list of coins given the amount of money in
;;   in cents, rounded to the nearest 5 cents
;; make-change: Nat -> (listof Sym)
;; examples:
;(check-expect (make-change 11) (cons 'dime empty))
;(check-expect (make-change 17) (cons 'dime (cons 'nickel empty)))

(define (make-change cents)
  (local
    [(define num-toonie (quotient (+ cents 2) toonie))
     (define num-loonie (quotient (+ (- cents (* toonie num-toonie)) 2)
                                  loonie))
     (define num-quarter (quotient (+ (- cents (+ (* toonie num-toonie)
                                                  (* loonie num-loonie))) 2)
                                   quarter))
     (define num-dime (quotient (+ (- cents (+ (* toonie num-toonie)
                                               (* loonie num-loonie)
                                               (* quarter num-quarter))) 2)
                                dime))
     (define num-nickel (quotient (+ (- cents (+ (* toonie num-toonie)
                                                 (* loonie num-loonie)
                                                 (* quarter num-quarter)
                                                 (* dime num-dime))) 2)
                                  nickel))
     (define list-toonie (map first
                              (build-list num-toonie
                                          (lambda (x) (list 'toonie x)))))
     (define list-loonie (map first
                              (build-list num-loonie
                                          (lambda (x) (list 'loonie x)))))
     (define list-quarter (map first
                               (build-list num-quarter
                                           (lambda (x) (list 'quarter x)))))
     (define list-dime (map first
                            (build-list num-dime
                                        (lambda (x) (list 'dime x)))))
     (define list-nickel (map first
                              (build-list num-nickel
                                          (lambda (x) (list 'nickel x)))))]
    (foldr cons list-nickel
           (foldr cons list-dime
                  (foldr cons list-quarter
                         (foldr cons list-loonie list-toonie))))))
                                
                            

                               
;; tests:
(check-expect (make-change 203) (cons 'toonie (cons 'nickel empty)))
(check-expect (make-change 207) (cons 'toonie (cons 'nickel empty)))
(check-expect (make-change 8) (cons 'dime empty))
(check-expect (make-change 12) (cons 'dime empty))
(check-expect (make-change 23) (cons 'quarter empty))
(check-expect (make-change 27) (cons 'quarter empty))
(check-expect (make-change 102) (cons 'loonie empty))
(check-expect (make-change 3) (cons 'nickel empty))
(check-expect (make-change 7) (cons 'nickel empty))
(check-expect (make-change 198) (cons 'toonie empty))
(check-expect (make-change 202) (cons 'toonie empty))
(check-expect (make-change 98) (cons 'loonie empty))
(check-expect (make-change 2) empty)
(check-expect (make-change 0) empty)
(check-expect (make-change 1392) '(toonie
                                   toonie
                                   toonie
                                   toonie
                                   toonie
                                   toonie
                                   loonie
                                   quarter
                                   quarter
                                   quarter
                                   dime
                                   nickel))
(check-expect (make-change 340)
              '(toonie loonie quarter dime nickel))
                 