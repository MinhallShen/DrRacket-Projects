;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 2 Question 1
;; ***************************************************
;;

; 1a)

(define (q1a-alt n a?)
  (cond
    [(and a? (>= n 0)) (add1 n)]
    [(and a? (< n 0)) (sub1 n)]
    [else 0]))

; 1b)

(define (q1b-alt a? b? c?)
  (cond
    [(and a? b?) 'elm]
    [(and a? c?) 'cedar]
    [a?'birch]
    [b? 'pine]
    [(not c?) 'birch]
    [else 'cherry]))

; 1c)

(define (q1c-alt a? b? c?)
  (cond
    [a? 'oak]
    [(and c? b?) 'maple]
    [c? 'willow]
    [b? 'buckthorn]
    [else 'sumac]))
    
; 1d)
(define (q1d-alt a? b? c?)
  (cond
    [(or (and a? b? (not c?))
         (and a? (not b?) c?)
         (and a? (not b?) (not c?))) 'hazel]
    [(and (not a?)
          (not b?)
          c?) 'hickory]
    [(not (or a? b? c?)) 'larch]
    [else 'spruce]))




    



  
    
