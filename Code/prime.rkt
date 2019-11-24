;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname prime) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 4 Question 3
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; a)
;; (num-factors num base) produces a list of all the positive factors between
;;   two numbers
;; num-factors: Nat Nat -> (listof Nat)
;; examples:
(check-expect (num-factors 10 2) (cons 2 (cons 5 (cons 10 empty))))
(check-expect (num-factors 6 1) (cons 1 (cons 2 (cons 3 (cons 6 empty)))))

(define (num-factors num base)
  (cond
    [(= (+ num 1) base) empty]
    [(= (remainder num base) 0)
     (cons base (num-factors num (add1 base)))]
    [else (num-factors num (add1 base))]))

;; (prime? number) returns true if the number is prime, and false otherwise.
;; prime?: Nat -> Bool
;; examples:
(check-expect (prime? 10) false)
(check-expect (prime? 3) true)

(define (prime? number)
  (cond
    [(or (= number 0) (= number 1)) false]
    [(= (length (num-factors number 1)) 2) true]
    [else false]))

;; tests:
(check-expect (prime? 2) true)
(check-expect (prime? 0) false)
(check-expect (prime? 1) false)
(check-expect (prime? 8) false)
(check-expect (prime? 37) true)

;; b)
;; (next-prime num) produces the next prime number after some natural number num
;; next-prime: Nat -> Nat
;; examples:
(check-expect (next-prime 3) 5)
(check-expect (next-prime 8) 11)

(define (next-prime num)
  (cond
    [(prime? (add1 num)) (add1 num)]
    [else (next-prime (add1 num))]))

;; tests:
(check-expect (next-prime 10) 11)
(check-expect (next-prime 13) 17)
(check-expect (next-prime 0) 2)
(check-expect (next-prime 1) 2)
(check-expect (next-prime 5) 7)

;; c)
;; (prime-range start end) produces a list of all the prime numbers between a
;;   start number and end number, inclusive.
;; next-prime: Nat Nat -> (listof Nat)
;; examples:
(check-expect (prime-range 2 16)
              (cons 2 (cons 3 (cons 5 (cons 7 (cons 11 (cons 13 empty)))))))
(check-expect (prime-range 1 10)
              (cons 2 (cons 3 (cons 5 (cons 7 empty)))))

(define (prime-range start end)
  (cond
    [(< end start) empty]
    [(prime? start) (cons start (prime-range (add1 start) end))]
    [else (prime-range (add1 start) end)]))

;; tests:
(check-expect (prime-range 2 7)
              (cons 2 (cons 3 (cons 5 (cons 7 empty)))))
(check-expect (prime-range 0 13)
              (cons 2 (cons 3 (cons 5 (cons 7 (cons 11 (cons 13 empty)))))))
(check-expect (prime-range 4 11)
              (cons 5 (cons 7 (cons 11 empty))))
(check-expect (prime-range 11 4) empty)
(check-expect (prime-range 2 2) (cons 2 empty))
(check-expect (prime-range 4 4) empty)
(check-expect (prime-range 0 2) (cons 2 empty))




     
    
      


    