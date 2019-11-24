;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname change) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 4 Question 3
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; a)
;; (count-change coin-list) calculates how much change there is in cents given
;;   a list of different Canadian coins.
;; count-change: (listof Sym) -> Nat)
;; examples:
(check-expect (count-change (cons 'dime (cons 'quarter empty))) 35)
(check-expect (count-change (cons 'loonie (cons 'toonie empty))) 300)

(define (count-change coin-list)
  (cond
    [(empty? coin-list) 0]
    [else (+ (cond
                [(symbol=? (first coin-list) 'nickel) 5]
                [(symbol=? (first coin-list) 'dime) 10]
                [(symbol=? (first coin-list) 'quarter) 25]
                [(symbol=? (first coin-list) 'loonie) 100]
                [(symbol=? (first coin-list) 'toonie) 200]
                [else 0])
              (count-change (rest coin-list)))]))

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

;; b)
;; (make-change cents) produces a list of coins given the amount of money in
;;   in cents, rounded to the nearest 5 cents
;; make-change: Nat -> (listof Sym)
;; examples:
(check-expect (make-change 11) (cons 'dime empty))
(check-expect (make-change 17) (cons 'dime (cons 'nickel empty)))

(define (make-change cents)
  (cond
    [(<= cents 2) empty]
    [else (cond
            [(>= cents 198) (cons 'toonie (make-change (- cents 200)))]
            [(>= cents 98) (cons 'loonie (make-change (- cents 100)))]
            [(>= cents 23) (cons 'quarter (make-change (- cents 25)))]
            [(>= cents 8) (cons 'dime (make-change (- cents 10)))]
            [(>= cents 3) (cons 'nickel (make-change (- cents 5)))])]))

;; tests:
(check-expect (make-change 203) (cons 'toonie (cons 'nickel empty)))
(check-expect (make-change 98) (cons 'loonie empty))
(check-expect (make-change 2) empty)
(check-expect (make-change 0) empty)
(check-expect (make-change 340) (cons 'toonie
                                      (cons 'loonie
                                            (cons 'quarter
                                                  (cons 'dime
                                                        (cons 'nickel empty))))
                                      ))
              



    
  