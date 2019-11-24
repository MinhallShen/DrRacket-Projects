;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname MT2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct s (a b c))
(define a 5)
(define b (+ a 5))
(define (f x) (+ x a b))
(define q (make-s 5 false 'statement))
(define (dup x) (list x x))
(define (what s) (list->string (rest (rest (string->list s)))))
(define (silly x n) (cond [(empty? x) x]
[(even? n) (silly (rest x) (add1 n))]
[(odd? n) (cons (first x) (silly (rest x) (add1 n)))]))
(define s0 (silly (list 1 2 3 4 5 6 7 8) 0))
(define s1 (silly (list 1 2 3 4 5 6 7 8) 1))
(define q1 (list 1 (list 2 3) (list 4)))

(define-struct pair (x y))
;; A Pair is a (make-pair Any Any)
(define p (make-pair (make-pair (list 1 2 3) (list ’a ’b ’c)) "three" ))