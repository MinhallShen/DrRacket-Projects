;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname duplicates) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 4 Question 2
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; (remove-duplicates list-of-num) produces list-of-num without any duplicates
;;   in the same order.
;; remove-duplicates: (listof Num) -> (listof Num)
;; examples:
(check-expect (remove-duplicates (cons 1
                                       (cons 3
                                             (cons 1
                                                   (cons 2
                                                         (cons 4
                                                               (cons 2
                                                                     (cons 7
                                                (cons 2 (cons 5 empty))))))))))
              (cons 3 (cons 1 (cons 4 (cons 7 (cons 2 (cons 5 empty)))))))
(check-expect (remove-duplicates (cons 2
                                       (cons 2
                                             (cons 2
                                                   (cons 4
                                                         (cons 5
                                                               (cons 4 empty)))
                                                   ))))
              (cons 2 (cons 5 (cons 4 empty))))

(define (remove-duplicates list-of-num)
  (cond
    [(empty? list-of-num) empty]
    [else (cond
           [(member? (first list-of-num) (rest list-of-num))
            (remove-duplicates (rest list-of-num))] 
           [else (cons (first list-of-num)
                       (remove-duplicates (rest list-of-num)))])]))

;; tests:
(check-expect (remove-duplicates (cons 1 (cons 1 (cons 1 (cons 1 empty)))))
              (cons 1 empty))
(check-expect (remove-duplicates empty)
              empty)
(check-expect (remove-duplicates (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
              (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
(check-expect (remove-duplicates (list -1 1 -1))
              (cons 1 (cons -1 empty)))

              
       
            
            
            
            
                 