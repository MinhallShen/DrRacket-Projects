;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname mobster) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 6 Question 3
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; constants:
(define-struct goon (street-name abilities))
;; A Goon is a (make-goon Str Abilities).
;; An Abilities is a (list Nat Nat Nat), where the elements represent
;; Loyalty, Wealth, and Influence.a

(define goon-btt (make-goon "Bullet Tooth Tony" (list 8 2 5)))
(define goon-ca (make-goon "Cousin Avi" (list 3 9 8)))
(define goon-btb (make-goon "Boris, the Blade" (list 4 6 7)))
(define goon-fff (make-goon "Franky Four Fingers" (list 5 7 3)))
(define goon-me (make-goon "Minhall" (list 100 100 100)))
(define applicant-gg (make-goon "Gorgeous George" (list 3 5 4)))
(define applicant-s (make-goon "Sol" (list 5 7 10)))


;; A Gang is a (listof Goon)
(define my-gang (list goon-btt goon-ca goon-btb goon-fff))
;; A Job is a (list Nat Nat Nat), where the elements represent
;; required Loyalty, required Wealth, and required Influence.

(define job-mule (list 5 0 1))
(define job-financer (list 3 8 5))
(define job-bribe (list 5 6 10))
;; A Job-list is a (listof Job)

(define my-jobs (list job-mule job-financer job-bribe))

;------------------------------------------------------------------------------

;; a)
;; (meet-req? goon job) consumes a goon and a job and returns true if the goon's
;;   abilities meet the job's requirements and false otherwise.
;; meet-req?: Goon Job -> Bool
;; examples:
(check-expect (meet-req? goon-ca job-mule) false)
(check-expect (meet-req? goon-me job-financer) true)

(define (meet-req? goon job)
  (cond
    [(and (<= (first job) (first (goon-abilities goon)))
          (<= (second job) (second (goon-abilities goon)))
          (<= (third job) (third (goon-abilities goon))))
     true]
    [else false]))

;; (add-list lst) consumes a list of natural numbers and produces the sum of the
;;   numbers in the list.
;;  add-list: (listof Nat) -> Nat
;; examples:
(check-expect (add-list '(1 2 3)) 6)
(check-expect (add-list '()) 0)

(define (add-list lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst) (add-list (rest lst)))]))

;; examples:
(check-expect (eval-goon goon-ca job-mule) false)
(check-expect (eval-goon goon-ca job-financer) 4)

;; (eval-goon goon job) consumes a Goon and Job and returns the difference
;;   between the sum of the goon's abilities and the sum of the job's
;;   requirements and returns false if the goon does not meet the requirements.
;; eval-goon: Goon Job -> (anyof Nat false)
;; examples:
(check-expect (eval-goon goon-ca job-mule) false)
(check-expect (eval-goon goon-ca job-financer) 4)

(define (eval-goon goon job)
  (cond
    [(not (meet-req? goon job)) false]
    [else (- (add-list (goon-abilities goon))
             (add-list job))]))

;; tests:
(check-expect (eval-goon goon-fff job-mule) 9)
(check-expect (eval-goon goon-ca job-mule) false)
(check-expect (eval-goon goon-ca job-financer) 4)

;------------------------------------------------------------------------------

;; b)
;; (eval-goon2 goon job) consumes a Goon and Job and returns the difference
;;   between the sum of the goon's abilities and the sum of the job's
;;   requirements.
;; eval-goon2: Goon Job -> Nat
;; examples:
(check-expect (eval-goon2 goon-ca job-mule) 14)
(check-expect (eval-goon2 goon-ca job-financer) 4)

(define (eval-goon2 goon job)
  (- (add-list (goon-abilities goon))
     (add-list job)))

;; (qualified-gang gang job) consunmes a Goon and a Job and returns a list of
;;   Goons that are qualified for the job.
;; qualified-gang: Goon Job -> (listof Goon)
;; examples:
(check-expect (qualified-gang my-gang job-mule)
              (list goon-btt goon-fff))
(check-expect (qualified-gang empty job-mule) empty)

(define (qualified-gang gang job)
  (cond
    [(empty? gang) empty]
    [(meet-req? (first gang) job)
     (cons (first gang) (qualified-gang (rest gang) job))]
    [else (qualified-gang (rest gang) job)]))

;; (pick-goon/acc gang best-so-far job) produces the most qualified member from
;;   a gang to perform a job.
;; pick-goon/acc: Gang Goon Job -> Goon
;; example:
(check-expect (pick-goon my-gang job-mule) goon-btt)

(define (pick-goon/acc gang best-so-far job)
  (cond
    [(empty? gang) best-so-far]
    [(> (eval-goon2 (first gang) job)
        (eval-goon2 best-so-far job))
     (pick-goon/acc (rest gang) (first gang) job)]
    [else (pick-goon/acc (rest gang) best-so-far job)]))

;; (pick-goon gang job) consumes a Gang and Job and picks the best qualified
;;   goon to perform the job, and produces false if there are not qualified
;;   goons.
;; pick-goon: Gang Job -> (anyof Goon false)
;; examples:
(check-expect (pick-goon my-gang job-mule) goon-btt)
(check-expect (pick-goon my-gang job-financer) goon-ca)
(check-expect (pick-goon my-gang job-bribe) false)

(define (pick-goon gang job)
  (cond
    [(empty? (qualified-gang gang job)) false]
    [else (pick-goon/acc (rest (qualified-gang gang job))
                         (first (qualified-gang gang job)) job)]))


;; tests:
(check-expect (pick-goon empty job-financer) false)
(check-expect (pick-goon my-gang job-bribe) false)
(check-expect (pick-goon my-gang job-mule) goon-btt)
(check-expect (pick-goon (list (make-goon "a" '(10 10 10))
                               (make-goon "b" '(0 1 0))
                               (make-goon "c" '(11 11 11))) job-mule)
              (make-goon "c" '(11 11 11)))
  
;------------------------------------------------------------------------------

;; c)

;; (find-difficult-jobs gang job-list) consumes a Gang and a list of jobs and
;;   returns a list of jobs that no one in the gang can handle.
;; find-difficult-job: Gang (listof Job) -> (listof Job)
;; example:
(check-expect (find-difficult-jobs my-gang my-jobs) (list job-bribe))

(define (find-difficult-jobs gang job-list)
  (cond
    [(empty? job-list) empty]
    [(false? (pick-goon gang (first job-list)))
     (cons (first job-list) (find-difficult-jobs (rest gang) (rest job-list)))]
    [else (find-difficult-jobs (rest gang) (rest job-list))]))

;; tests:
(check-expect (find-difficult-jobs my-gang my-jobs) (list job-bribe))
(check-expect (find-difficult-jobs (list (make-goon "a" '(10 10 10))
                                         (make-goon "b" '(0 1 0))
                                         (make-goon "c" '(11 11 11)))
                                   my-jobs) empty)

;------------------------------------------------------------------------------

;; d)

;; (qualified-for-any? job-list applicant) consumes a list of jobs and a goon
;;   and returns true if the goon meets the requirements of any of the jobs in
;;   the list of jobs and false otherwise.
;; qualified-for-any?: (listof Job) Goon -> Bool
;; examples:
(check-expect (qualified-for-any? my-jobs applicant-gg) false)
(check-expect (qualified-for-any? my-jobs goon-me) true)

(define (qualified-for-any? job-list applicant)
  (cond
    [(empty? job-list) false]
    [(meet-req? applicant (first job-list)) true]
    [else (qualified-for-any? (rest job-list) applicant)]))

;; (hire? gang job-list applicant) consumes a Gang, list of jobs and a Goon,
;;   representing the applicant and produces true if the applicant is qualified
;;   to do a job that none of the current gang, and false otherwise.
;; hire?: Gang (listof Job) Goon -> Bool
;; examples:
(check-expect (hire? my-gang my-jobs applicant-gg) false)
(check-expect (hire? my-gang my-jobs applicant-s) true)

(define (hire? gang job-list applicant)
  (cond
    [(empty? (find-difficult-jobs gang job-list)) false]
    [(qualified-for-any? job-list applicant) true]
    [else false]))

;; tests:
(check-expect (hire? my-gang my-jobs applicant-gg) false)
(check-expect (hire? my-gang my-jobs applicant-s) true)
(check-expect (hire? my-gang (list '(11 11 11)) applicant-s) false)
(check-expect (hire? (list (make-goon "a" '(10 10 10))
                                         (make-goon "b" '(0 1 0))
                                         (make-goon "c" '(11 11 11)))
                     (list '(11 11 11)) goon-me) false)
(check-expect (hire? (list (make-goon "a" '(10 10 10))
                                         (make-goon "b" '(0 1 0))
                                         (make-goon "c" '(11 11 11)))
                     (list '(11 11 12)) goon-me) true)





























