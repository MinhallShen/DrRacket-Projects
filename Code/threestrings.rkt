;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname threestrings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 3 Question 3
;; ***************************************************
;;
;-----------------------------------------------------------------------------

;; a)
;; (in-order? s1 s2 s3) consumes three strings and returns true if the three
;;   strings are in alphabetical order, and false if they are not.
;; in-order?: Str Str Str -> Bool
;; examples:
(check-expect (in-order? "a" "c" "b") false)
(check-expect (in-order? "a" "b" "c") true)

(define (in-order? s1 s2 s3)
  (cond
    [(and (string<=? s1 s2) (string<=? s2 s3)) true]
    [else false]))

;; tests:
(check-expect (in-order? "a" "b" "c") true)
(check-expect (in-order? "a" "c" "b") false)
(check-expect (in-order? "a" "a" "a") true)
(check-expect (in-order? "a" "a" "b") true)

;-----------------------------------------------------------------------------

;; b)
;; (sort3 stringlist) consumes a list of three strings and produces the same
;;   list of strings in alphabetical order.
;; sort3: (listof Str) -> (listof Str)
;; requires: stringlist length is 3
;; examples:
(check-expect
(sort3 (cons "b" (cons "b" (cons "c" empty))))
(cons "b" (cons "b" (cons "c" empty))))
(check-expect
(sort3 (cons "c" (cons "b" (cons "a" empty))))
(cons "a" (cons "b" (cons "c" empty))))
(check-expect
(sort3 (cons "a" (cons "b" (cons "a" empty))))
(cons "a" (cons "a" (cons "b" empty))))

(define (sort3 stringlist)
  (cond
    [(in-order? (first stringlist)
                (second stringlist)
                (third stringlist))
     stringlist]
    [(in-order? (first stringlist)
                (third stringlist)
                (second stringlist))
     (cons (first stringlist)
           (cons (third stringlist)
           (cons (second stringlist) empty)))]
    [(in-order? (second stringlist)
                (first stringlist)
                (third stringlist))
     (cons (second stringlist)
            (cons (first stringlist)
                  (cons (third stringlist) empty)))]
    [(in-order? (second stringlist)
                (third stringlist)
                (first stringlist))
     (cons (second stringlist)
            (cons (third stringlist)
                  (cons (first stringlist) empty)))]
    [(in-order? (third stringlist)
                (first stringlist)
                (second stringlist))
     (cons (third stringlist)
            (cons (first stringlist)
                  (cons (second stringlist) empty)))]
    [else
     (cons (third stringlist)
            (cons (second stringlist)
                  (cons (first stringlist) empty)))]))

;; tests:
(check-expect (sort3 (cons "a" (cons "b" (cons "c" empty))))
              (cons "a" (cons "b" (cons "c" empty))))

(check-expect (sort3 (cons "a" (cons "c" (cons "b" empty))))
              (cons "a" (cons "b" (cons "c" empty))))

(check-expect (sort3 (cons "b" (cons "a" (cons "c" empty))))
              (cons "a" (cons "b" (cons "c" empty))))

(check-expect (sort3 (cons "b" (cons "c" (cons "a" empty))))
              (cons "a" (cons "b" (cons "c" empty))))

(check-expect (sort3 (cons "c" (cons "a" (cons "b" empty))))
              (cons "a" (cons "b" (cons "c" empty))))

(check-expect (sort3 (cons "c" (cons "b" (cons "a" empty))))
              (cons "a" (cons "b" (cons "c" empty))))

(check-expect (sort3 (cons "b" (cons "a" (cons "b" empty))))
              (cons "a" (cons "b" (cons "b" empty))))

(check-expect (sort3 (cons "b" (cons "b" (cons "a" empty))))
              (cons "a" (cons "b" (cons "b" empty))))

(check-expect (sort3 (cons "a" (cons "b" (cons "b" empty))))
              (cons "a" (cons "b" (cons "b" empty))))

(check-expect (sort3 (cons "a" (cons "a" (cons "a" empty))))
              (cons "a" (cons "a" (cons "a" empty))))

;-----------------------------------------------------------------------------               

;; c)
;; (find-second stringlist2) consumes a list of three strings and returns the
;;   second largest string lexicographically.
;; find-second: (listof Str) -> (anyof Str empty)
;; requires: stringlist2 is length 3
;; examples: 
(check-expect (find-second (cons "c" (cons "a" (cons "b" empty))))
"b")
(check-expect (find-second (cons "c" (cons "c" (cons "a" empty))))
"a")
(check-expect (find-second (cons "a" (cons "a" (cons "a" empty))))
empty)
(check-expect (find-second (cons "c" (cons "a" (cons "a" empty))))
"a")

(define (find-second stringlist2)
  (cond
    [(string=? (first stringlist2)
        (second stringlist2)
        (third stringlist2)) empty]
    [(and (string=? (first stringlist2) (second stringlist2))
          (string>? (first stringlist2)
                        (third stringlist2)))
     (third stringlist2)]
    [(and (string=? (first stringlist2) (third stringlist2))
          (string>? (first stringlist2)
                    (second stringlist2)))
     (second stringlist2)]
    [(and (string=? (second stringlist2) (third stringlist2))
          (string>? (second stringlist2)
                    (first stringlist2)))
     (first stringlist2)]          
    [else
     (second (sort3 stringlist2))]))

;; tests:
(check-expect (find-second (cons "a" (cons "b" (cons "c" empty)))) "b")
(check-expect (find-second (cons "a" (cons "c" (cons "b" empty)))) "b")
(check-expect (find-second (cons "b" (cons "a" (cons "c" empty)))) "b")
(check-expect (find-second (cons "a" (cons "a" (cons "b" empty)))) "a")
(check-expect (find-second (cons "a" (cons "b" (cons "b" empty)))) "a")
(check-expect (find-second (cons "b" (cons "a" (cons "a" empty)))) "a")
(check-expect (find-second (cons "a" (cons "b" (cons "a" empty)))) "a")
(check-expect (find-second (cons "b" (cons  "a" (cons "b" empty)))) "a")
(check-expect (find-second (cons "a" (cons "a" (cons "a" empty)))) empty)

            
                
            
             
                            
            
                   
                            
                                   
    
                  
       