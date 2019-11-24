;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname manipulations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 9 Question 4
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; a)

;; (rotate-right lst) consumes a list and produces a list that moves the last
;;    element of a list to the front, with all other elements unchanged.
;; rotate-right: (listof Any) -> (listof Any)
;; examples:
(check-expect (rotate-right '(a b c d e f)) '(f a b c d e))
(check-expect (rotate-right '(a))'(a))
(check-expect (rotate-right empty) empty)

(define (rotate-right lst)
  (local
    [(define (last-element lst)
       (first (foldl cons empty lst)))
     (define (all-but-last lst)
       (foldl cons empty (rest (foldl cons empty lst))))]
    (cond
      [(empty? lst) empty]
      [else (cons (last-element lst) (all-but-last lst))])))

;; tests:
(check-expect (rotate-right '(a b c d e f)) '(f a b c d e))
(check-expect (rotate-right '(a))'(a))
(check-expect (rotate-right empty) empty)

;; b)

;; (rotate-left lst) consumes a list and produces a list that moves the first
;;    element of the list to the end of the list with all other elements
;;    unchanged.
;; examples:
(check-expect (rotate-left '(a b c d e f)) '(b c d e f a))
(check-expect (rotate-left '(a))'(a))
(check-expect (rotate-left empty) empty)

(define (rotate-left lst)
  (cond
    [(empty? lst) empty]
    [else (foldr cons (cons (first lst) empty) (rest lst))]))

;; tests:
(check-expect (rotate-left '(a b c d e f)) '(b c d e f a))
(check-expect (rotate-left '(a))'(a))
(check-expect (rotate-left empty) empty)

;; (prefix n lst) consumes a natural number and a list and produces a list of
;;   the first n elements of the list.
;; prefix: Nat (listof Any) -> (listof Any)
;; examples:

(define (prefix n lst)
  (local [(define list-length
            (foldr (lambda (dont-care ans)
                     (+ 1 ans))
                   0 lst))
          (define make-al
            (map (lambda (num1 num2) (cons num1 (cons num2 empty)))
                 (build-list list-length (lambda (x) x))
                 lst))]
          
    (map second (filter (lambda (item) (< (first item) n)) make-al))))
          
    
          
(check-expect (prefix 3 '(a b c d e f)) '(a b c))
(check-expect (prefix 10 '(a b c d e f)) '(a b c d e f))
(check-expect (prefix 0 '(a b c d e f)) empty)
(check-expect (prefix 1 '(a b c d e f)) '(a))
  

     
  
