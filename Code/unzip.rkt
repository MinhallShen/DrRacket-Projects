;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname unzip) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 8 Question 2
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; (unzip lop) consumes a list of pairs and produces a list of two lists where
;;   the first list contains the first element from each pair and the second
;;   list contains the second element from each pair.
;; unzip: (listof (listof Any)) -> (listof (listof Any))
;; Requires: the length of the lists in lop must be 2
;; examples:
(check-expect (unzip '((1 "a") (2 b) ("3" a))) '((1 2 "3") ("a" b a)))
(check-expect (unzip empty) '(()()))

(define (unzip lop)
  (local
    [;; (lst-first-element lop) consumes a list of pairs and produces a list 
     ;;   containing the first element from each pair.
     ;; lst-first-element: (listof (listof Any)) -> (listof Any)
     (define (lst-first-element lop)
       (cond
         [(empty? lop) empty]
         [else (cons (first (first lop)) (lst-first-element (rest lop)))]))
     (;; (lst-first-element lop) consumes a list of pairs and produces a list 
      ;;   containing the second element from each pair.
      ;; lst-first-element: (listof (listof Any)) -> (listof Any)
      define (lst-second-element lop)
       (cond
         [(empty? lop) empty]
         [else (cons (second (first lop)) (lst-second-element (rest lop)))]))]
    (list (lst-first-element lop) (lst-second-element lop))))

;; tests:
(check-expect (unzip empty) '(()()))
(check-expect (unzip '((1 "a") (2 b) ("3" a))) '((1 2 "3") ("a" b a)))
(check-expect (unzip '((ok 1) (four 4) ("five" 6) (asdf 9)))
                     '((ok four "five" asdf)(1 4 6 9)))