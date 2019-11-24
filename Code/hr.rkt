;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 6 Question 2
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; structure definition:
(define-struct pr (first-name last-name))
;; A personnel record (PR) is a (make-pr Str Str).

;; constants:
(define ed-kitchener (list (make-pr "Gord" "Downie")
                           (make-pr "Rob" "Baker")
                           (make-pr "Gord" "Sinclair")
                           (make-pr "Paul" "Langlois")
                           (make-pr "Johnny" "Fay")))

(define ed-waterloo (list (make-pr "Byron" "Stroud")
                          (make-pr "Jed" "Simon")
                          (make-pr "Gord" "Downie")
                          (make-pr "Devin" "Townsend")
                          (make-pr "Rob" "Baker")))

;; a)
;; i.
;; (pr=? pr1 pr2) consumes two personnel records and produces true if they are
;;   the have the same name and false otherwise
;; pr=?: PR PR -> Bool
;; examples:
(check-expect (pr=? (make-pr "Gord" "Down") (make-pr "Gord" "Down")) true)
(check-expect (pr=? (make-pr "Gord" "Down") (make-pr "Dord" "Gown")) false)

(define (pr=? pr1 pr2)
  (cond [(and (string=? (pr-first-name pr1) (pr-first-name pr2))
              (string=? (pr-last-name pr1) (pr-last-name pr2))) true]
        [else false]))

;; tests:
(check-expect (pr=? (make-pr "Gord" "Down") (make-pr "Gord" "Down")) true)
(check-expect (pr=? (make-pr "Gord" "Down") (make-pr "Dord" "Gown")) false)

;; ii.
;; (pr<? pr1 pr2 sym) consumes two PRs and a symbol 'first or 'last and compares
;;   the first or last name depending on the symbol and returns true if the
;;   first PR comes alphabetically before the second and false otherwise.
;; pr<?: PR PR Sym -> Bool
;; requires: sym must be (oneof 'first 'last)
;; examples:
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gard" "Duwn") 'first) false)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gard" "Duwn") 'last) true)

(define (pr<? pr1 pr2 sym)
  (cond
    [(symbol=? sym 'first)
     (cond
       [(string<? (pr-first-name pr1) (pr-first-name pr2)) true]
       [else false])]
    [else
     (cond
       [(string<? (pr-last-name pr1) (pr-last-name pr2)) true]
       [else false])]))

;; tests:
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gard" "Duwn") 'first) false)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gard" "Duwn") 'last) true)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gord" "Dawn") 'last) false)
(check-expect
 (pr<? (make-pr "Gord" "Dawn") (make-pr "Gyrd" "Dawn") 'first) true)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gord" "Dawn") 'first) false)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gord" "Down") 'first) false)

;; iii.
;; (pr<? pr1 pr2 sym) consumes two PRs and a symbol 'first or 'last and compares
;;   the first or last name depending on the symbol and returns true if the
;;   first PR comes alphabetically after the second and false otherwise.
;; pr>?: PR PR Sym -> Bool
;; requires: sym must be (oneof 'first 'last)
;; examples:
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gard" "Duwn") 'first) true)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gard" "Duwn") 'last) false)

(define (pr>? pr1 pr2 sym)
  (cond
    [(symbol=? sym 'first)
     (cond
       [(string>? (pr-first-name pr1) (pr-first-name pr2)) true]
       [else false])]
    [else
     (cond
       [(string>? (pr-last-name pr1) (pr-last-name pr2)) true]
       [else false])]))

;; tests:
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gard" "Duwn") 'first) true)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gard" "Duwn") 'last) false)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gord" "Dawn") 'last) true)
(check-expect
 (pr>? (make-pr "Gord" "Dawn") (make-pr "Gyrd" "Dawn") 'first) false)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gord" "Dawn") 'first) false)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gord" "Down") 'first) false)

;------------------------------------------------------------------------------

;; b)
;; (insert-ed pr ED sym) consumes a PR, ED and symbol 'first or 'last and
;;   inserts PR into the last slot of ED.
;; insert-ed: PR ED Sym -> ED
;; requires: sym is (oneof 'first 'last)
;; example:
(check-expect (insert-ed (make-pr "a" "b") ed-kitchener 'last)
              (list (make-pr "Gord" "Downie")
                    (make-pr "Rob" "Baker")
                    (make-pr "Gord" "Sinclair")
                    (make-pr "Paul" "Langlois")
                    (make-pr "Johnny" "Fay")
                    (make-pr "a" "b")))

(define (insert-ed pr ED sym)
  (cond
    [(symbol=? sym 'first)
     (cond
       [(empty? ED) (cons pr empty)]
       [(string<? (pr-first-name pr) (pr-first-name (first ED)))
        (cons pr ED)]
       [else (cons (first ED)
                   (insert-ed pr (rest ED) 'first))])]
    [(symbol=? sym 'last)
     (cond
       [(empty? ED) (cons pr empty)]
       [(string<? (pr-last-name pr) (pr-last-name (first ED)))
        (cons pr ED)]
       [else (cons (first ED)
                   (insert-ed pr (rest ED) 'last))])]))

;; (sort-ed ED sym) consumes a ED and a symbol 'first or 'last and produces an
;;   ED sorted by first or last name depending on the symbol.
;; sort-ed: ED Sym -> ED
;; requires: sym is (oneof 'first 'last)
;; examples:
(check-expect (sort-ed ed-waterloo 'first)
              (list (make-pr "Byron" "Stroud")
                    (make-pr "Devin" "Townsend")
                    (make-pr "Gord" "Downie")
                    (make-pr "Jed" "Simon")
                    (make-pr "Rob" "Baker")))

(check-expect (sort-ed ed-kitchener 'last)
              (list (make-pr "Rob" "Baker")
                    (make-pr "Gord" "Downie")
                    (make-pr "Johnny" "Fay")
                    (make-pr "Paul" "Langlois")
                    (make-pr "Gord" "Sinclair")))

(check-expect (sort-ed (list (make-pr "A" "B")
                             (make-pr "A" "B")) 'last)
              (list (make-pr "A" "B")
                    (make-pr "A" "B")))

(check-expect (sort-ed (list (make-pr "B" "B")
                             (make-pr "A" "B")) 'last)
              (list (make-pr "A" "B")
                    (make-pr "B" "B")))

(check-expect (sort-ed (list (make-pr "A" "B")
                             (make-pr "A" "A")) 'last)
              (list (make-pr "A" "A")
                    (make-pr "A" "B")))


(define (sort-ed ED sym)
  (cond
    [(symbol=? sym 'first)
     (cond
       [(empty? ED) empty]
       [else (insert-ed (first ED) (sort-ed (rest ED) 'first) 'first)])]
    [(symbol=? sym 'last)
     (cond
       [(empty? ED) empty]
       [else (insert-ed (first ED) (sort-ed (rest ED) 'last) 'last)])]))

;; tests:
(check-expect (sort-ed ed-waterloo 'first)
              (list (make-pr "Byron" "Stroud")
                    (make-pr "Devin" "Townsend")
                    (make-pr "Gord" "Downie")
                    (make-pr "Jed" "Simon")
                    (make-pr "Rob" "Baker")))

(check-expect (sort-ed ed-kitchener 'last)
              (list (make-pr "Rob" "Baker")
                    (make-pr "Gord" "Downie")
                    (make-pr "Johnny" "Fay")
                    (make-pr "Paul" "Langlois")
                    (make-pr "Gord" "Sinclair")))

;------------------------------------------------------------------------------

;; c)
;; (remove-duplicates ED) consumes a ED and produces a ED without duplicates.
;; remove-duplicates: ED -> ED
;; examples:
(check-expect (remove-duplicates (list (make-pr "Gord" "Downie")
                                       (make-pr "Rob" "Baker")
                                       (make-pr "Gord" "Sinclair")
                                       (make-pr "Paul" "Langlois")
                                       (make-pr "Johnny" "Fay")))
                                 (list (make-pr "Gord" "Downie")
                                       (make-pr "Rob" "Baker")
                                       (make-pr "Gord" "Sinclair")
                                       (make-pr "Paul" "Langlois")
                                       (make-pr "Johnny" "Fay")))

(check-expect (remove-duplicates (list (make-pr "Gord" "Downie")
                                       (make-pr "Rob" "Baker")
                                       (make-pr "Gord" "Sinclair")
                                       (make-pr "Paul" "Langlois")
                                       (make-pr "Johnny" "Fay")
                                       (make-pr "Gord" "Downie")))
                                 (list (make-pr "Rob" "Baker")
                                       (make-pr "Gord" "Sinclair")
                                       (make-pr "Paul" "Langlois")
                                       (make-pr "Johnny" "Fay")
                                       (make-pr "Gord" "Downie")))

(define (remove-duplicates ED)
  (cond
    [(empty? ED) empty]
    [else (cond
           [(member? (first ED) (rest ED))
            (remove-duplicates (rest ED))] 
           [else (cons (first ED)
                       (remove-duplicates (rest ED)))])]))

;; (merged-ED ED1 ED2) consumes two EDs and produces another ED containing
;;   elements of ED1 and ED2. Duplicates are not removed.
;; merged-ED: ED ED -> ED
;; examples:
(check-expect (merged-ED empty ed-kitchener) ed-kitchener)
(check-expect (merged-ED ed-waterloo empty) ed-waterloo)
(check-expect (merged-ED empty empty) empty)
(check-expect (merged-ED ed-waterloo ed-kitchener)
              (list
               (make-pr "Byron" "Stroud")
               (make-pr "Jed" "Simon")
               (make-pr "Gord" "Downie")
               (make-pr "Devin" "Townsend")
               (make-pr "Rob" "Baker")
               (make-pr "Gord" "Downie")
               (make-pr "Rob" "Baker")
               (make-pr "Gord" "Sinclair")
               (make-pr "Paul" "Langlois")
               (make-pr "Johnny" "Fay")))

(define (merged-ED ED1 ED2)
  (cond
    [(and (empty? ED1) (empty? ED2)) empty]
    [(empty? ED1) ED2]
    [(empty? ED2) ED1]
    [else
     (cons (first ED1) (merged-ED (rest ED1) ED2))]))

;; (merged-ed ED1 ED2) consumes two EDs and produces an ED containing elements
;;   of ED1 and ED2 sorted lexicographically by last name without duplicates.
;; merged-ed: ED ED -> ED
;; example:
(check-expect (merge-ed ed-kitchener ed-waterloo)
              (list (make-pr "Rob" "Baker")
                    (make-pr "Gord" "Downie")
                    (make-pr "Johnny" "Fay")
                    (make-pr "Paul" "Langlois")
                    (make-pr "Jed" "Simon")
                    (make-pr "Gord" "Sinclair")
                    (make-pr "Byron" "Stroud")
                    (make-pr "Devin" "Townsend")))

(define (merge-ed ED1 ED2)
  (remove-duplicates (sort-ed (merged-ED ED1 ED2) 'last)))

;; tests:
(check-expect (merge-ed ed-kitchener ed-waterloo)
              (list (make-pr "Rob" "Baker")
                    (make-pr "Gord" "Downie")
                    (make-pr "Johnny" "Fay")
                    (make-pr "Paul" "Langlois")
                    (make-pr "Jed" "Simon")
                    (make-pr "Gord" "Sinclair")
                    (make-pr "Byron" "Stroud")
                    (make-pr "Devin" "Townsend")))
(check-expect (merge-ed ed-kitchener empty)
              (list
               (make-pr "Rob" "Baker")
               (make-pr "Gord" "Downie")
               (make-pr "Johnny" "Fay")
               (make-pr "Paul" "Langlois")
               (make-pr "Gord" "Sinclair")))
(check-expect (merge-ed empty ed-kitchener)
              (list
               (make-pr "Rob" "Baker")
               (make-pr "Gord" "Downie")
               (make-pr "Johnny" "Fay")
               (make-pr "Paul" "Langlois")
               (make-pr "Gord" "Sinclair")))
(check-expect (merge-ed empty empty) empty)
  





                 