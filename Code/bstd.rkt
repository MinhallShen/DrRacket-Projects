;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bstd) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 7 Question 1
;; ***************************************************
;;
;------------------------------------------------------------------------------

(define-struct node (key val left right))
;; A Node is a (make-node Nat Sym BSTD BSTD)
;; requires: key > every key in left BSTD
;;           key < every key in right BSTD

;; A binary search tree dictionary (BSTD) is one of:
;; * empty
;; * Node


;;
;; Test case from assignment
;;

;; leaves
(define n1 (make-node 1 'a '() '()))
(define n4 (make-node 4 'g '() '()))
(define n6 (make-node 6 'o '() '()))
(define n9 (make-node 9 'x '() '()))
(define n14 (make-node 14 'z '() '()))

;; interior nodes
(define n12 (make-node 12 'y n9  n14))
(define  n7 (make-node 7 'd n6 n12))
(define  n3 (make-node 3 'b n1  n4))
(define  n5 (make-node 5 'o n3  n7))
(define root1 n5)


;; 1a)

;; (in-bstd? dict n) produces true if key n is in the BSTD and false otherwise.
;; in-bstd?: BSTD Nat -> Bool
;; examples:
(check-expect (in-bstd? root1 12) true)
(check-expect (in-bstd? root1 48) false)
            
(define (in-bstd? dict n)
  (cond
    [(empty? dict) false]
    [(= n (node-key dict)) true]
    [(< n (node-key dict)) (in-bstd? (node-left dict) n)]
    [else (in-bstd? (node-right dict) n)]))

;;(range-count dict low high) produces the number of keys that are >= low and <
;;   high in dict range-count: BSTD Nat Nat -> Nat
;; requires: low < high
;; examples:
(check-expect (range-count root1 7 12) 2)
(check-expect (range-count root1 13 14) 0)

(define (range-count dict low high)
  (cond
    [(empty? dict) 0]
    [(= low high) 0]
    [(in-bstd? dict low) (add1 (range-count dict (add1 low) high))]
    [else (range-count dict (add1 low) high)]))

;; tests:
(check-expect (range-count root1 20 30) 0)
(check-expect (range-count root1 10 12) 0)
(check-expect (range-count root1 10 13) 1)
(check-expect (range-count root1 12 13) 1)
(check-expect (range-count root1 4 8) 4)
(check-expect (range-count root1 0 15) 9)

;------------------------------------------------------------------------------

;; b)

;; (get-val dict n) returns the node-val associated to the key in the dict
;; get-val: BSTD Nat -> Sym
;; requires: n must be in dict
;; examples:
(check-expect (get-val root1 5) 'o)
(check-expect (get-val root1 14) 'z)

(define (get-val dict n)
  (cond
    [(= n (node-key dict)) (node-val dict)]
    [(< n (node-key dict)) (get-val (node-left dict) n)]
    [else (get-val (node-right dict) n)]))

;; (range-query dict low high) produces a list of values whose keys in the dict
;;   are in the range >= low and < high. The list of values produced are in
;;   ascending order by their key.
;; range-query: BSTD Nat Nat -> (listof Sym)
;; requires: low < high
;; examples:
(check-expect (range-query root1 5 7) '(o o))
(check-expect (range-query root1 2 3) '())

(define (range-query dict low high)
  (cond
    [(empty? dict) empty]
    [(= low high) empty]
    [(in-bstd? dict low)
     (cons (get-val dict low) (range-query dict (add1 low) high))]
    [else (range-query dict (add1 low) high)]))

;; tests:
(check-expect(range-query root1 20 30) '() )
(check-expect(range-query root1 10 12) '() )
(check-expect(range-query root1 10 13) '(y))
(check-expect(range-query root1 12 13) '(y))
(check-expect(range-query root1 4 8) '(g o o d))
(check-expect(range-query root1 0 15) '(a b g o o d x y z))