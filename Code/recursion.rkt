;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 5 Question 2
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; a)
;; (my-list-ref numlist index) consumes a list of numbers and an index and
;;   produces the element in the list at the desired index, and false if the
;;   index exceeds the length of the list.
;; my-list-ref: (listof Num) Nat -> (anyof Num false)
;; examples:
(check-expect (my-list-ref '(1 2 3 4) 0) 1)
(check-expect (my-list-ref '(5 4 3) 2) 3)
(check-expect (my-list-ref '(2) 20) false)

(define (my-list-ref numlist index)
  (cond [(>= index (length numlist)) false]
        [(= 0 index) (first numlist)]
        [else (my-list-ref (rest numlist) (sub1 index))]))

;; tests:
(check-expect (my-list-ref '(1 2 3 4 5) 0) 1)
(check-expect (my-list-ref '(1 3 5 7 -3) 4) -3)
(check-expect (my-list-ref '(2 4 6 8) 2) 6)
(check-expect (my-list-ref '(1 2 3) 3) false)

;------------------------------------------------------------------------------

;; b)
;; (zip numlist strlist) consumes a list of numbers and a list of strings and
;;   produces an association list with the elements in the number list as the
;;   keys and the elements in the string list as the values.
;; zip: (listof Num) (listof Str) -> (listof (listof Num Str))
;; Requires: (length numlist) = (length strlist)
;; examples:
(check-expect (zip '(1 2 3 4) '("a" "b" "c" "d"))
'((1 "a") (2 "b") (3 "c") (4 "d")))
(check-expect (zip empty empty) empty)

(define (zip numlist strlist)
  (cond [(empty? numlist) empty]
        [else (cons (list (first numlist)
                          (first strlist))
                    (zip (rest numlist)
                         (rest strlist)))]))

;; tests:
(check-expect (zip empty empty) empty)
(check-expect (zip '(1 2 3) '("a" "b" "c"))
              '((1 "a") (2 "b") (3 "c")))
(check-expect (zip '(2 4) '("ok" "yes"))
              '((2 "ok") (4 "yes")))

;------------------------------------------------------------------------------

;; c)
;; (count-symbol los sym) consumes a list of symbols and a symbol and counts
;;   how many times the symbol appears in the list of symbols.
;; count-symbol: (listof Sym) Sym -> Nat
;; examples:
(check-expect (count-symbol '(a b c) 'b) 1)
(check-expect (count-symbol '() 'a) 0)
(check-expect (count-symbol '(a a b a) 'a) 3)
              
(define (count-symbol los sym)
  (cond
    [(empty? los) 0]
    [else (+ (cond
               [(symbol=? (first los) sym) 1]
               [else 0])
             (count-symbol (rest los) sym))]))

;; (count-symbol/2D lolos symbol) consumes a list of list of symbols and a
;;   symbol and counts how many times the symbol appears in any list of symbols.
;; count-symbol/2D: (listof (listof Sym)) Sym -> Nat
;; examples:
(check-expect (count-symbol/2D '((a a b) () (a)) 'a) 3)
(check-expect (count-symbol/2D empty 'x) 0)

(define (count-symbol/2D lolos symbol)
  (cond [(empty? lolos) 0]
        [else (+ (count-symbol (first lolos) symbol)
                 (count-symbol/2D (rest lolos) symbol))]))

;; tests:
(check-expect (count-symbol/2D empty 'a) 0)
(check-expect (count-symbol/2D '((b b b) (b a c) (a h) (b)) 'b) 5)
(check-expect (count-symbol/2D '((b b b) (b a c) (a h) (b)) 'p) 0)
(check-expect (count-symbol/2D '((b b b) (b a c) (a h) (b)) 'a) 2)
(check-expect (count-symbol/2D '((a)) 'a) 1)
        
  



                    
                          
                          
                          
                                                
                    
                         
                          
              
                    
       
                    
              
                    
