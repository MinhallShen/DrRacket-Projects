;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname mod-calculator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; x*s congruent y (mod m)

(define s 0)
(define (mod-calc x y m s)
  (cond
    [(= s (sub1 m)) empty]
    [(= (modulo (- (* x s) y) m) 0)
     (cons s (mod-calc x y m (add1 s)))]
    [else (mod-calc x y m (add1 s))]))