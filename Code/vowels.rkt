;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname vowels) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 4 Question 1
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; (count-vowels/list loc) counts the number of occurrences of vowels in a list
;;   of characters
;; count-vowels: (listof Char) -> Nat
;; examples:
(check-expect (count-vowels/list (cons #\a (cons #\b empty))) 1)
(check-expect (count-vowels/list (cons #\b (cons #\b empty))) 0)

(define (count-vowels/list loc)
  (cond
    [(empty? loc) 0]
    [else (+ (cond [(or (char=? (first loc) #\a)
                        (char=? (first loc) #\e)
                        (char=? (first loc) #\i)
                        (char=? (first loc) #\o)
                        (char=? (first loc) #\u)
                        (char=? (first loc) #\A)
                        (char=? (first loc) #\E)
                        (char=? (first loc) #\I)
                        (char=? (first loc) #\O)
                        (char=? (first loc) #\U)) 1]
                   [else 0])
             (count-vowels/list (rest loc)))]))

;; (count-vowels str) counts the number of vowels in a string.
;; count-vowels: Str -> Nat
;; examples:
(check-expect (count-vowels "hello") 2)
(check-expect (count-vowels "okay!") 2)
(check-expect (count-vowels "Alright") 2)

(define (count-vowels str)
  (count-vowels/list (string->list str)))

;; tests:
(check-expect (count-vowels "") 0)
(check-expect (count-vowels "alien") 3)
(check-expect (count-vowels "BecaUse") 4)
(check-expect (count-vowels "aeiou") 5)
(check-expect (count-vowels "AEIOU") 5)
(check-expect (count-vowels "AEIOUaeiou") 10)