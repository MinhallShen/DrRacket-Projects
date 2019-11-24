;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname football) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 2 Question 3
;; ***************************************************
;;

;-----------------------------------------------------------------

;; 3a)
;; (intentional-grounding? pressure? pocket? throw-ball?) determines
;; whether or not a play is considered an intentional grounding
;; given whether or not there was pressure, the QB was in the inside
;; pocket or if the ball was thrown.
;; intentional-grounding?: Bool Bool Bool -> Bool
;; example:
(check-expect (intentional-grounding? #t #t #f) #f)

(define (intentional-grounding? pressure? pocket? throw-ball?)
  (and pressure? pocket? throw-ball?))

;; tests:
(check-expect (intentional-grounding? #t #t #t) #t)
(check-expect (intentional-grounding? #t #t #f) #f)
(check-expect (intentional-grounding? #t #f #t) #f)
(check-expect (intentional-grounding? #t #f #f) #f)
(check-expect (intentional-grounding? #f #t #t) #f)
(check-expect (intentional-grounding? #f #t #f) #f)
(check-expect (intentional-grounding? #f #f #t) #f)
(check-expect (intentional-grounding? #f #f #f) #f)

;-----------------------------------------------------------------

;; 3b)
;; (intentional-grounding-correct? pressure?
;;                                 pocket?
;;                                 throw-ball?
;;                                 eligible-receiver?))
;; determines whether or not a play is an intentional grounding
;; given whether or not there was pressure, the QB was in the
;; inside pocket, and whether or not the ball was thrown towards
;; an eligible teammate.
;; intentional-grounding-correct?: Bool Bool Bool Bool -> Bool
;; example:
(check-expect (intentional-grounding-correct? #t #t #t #t) #f)

(define (intentional-grounding-correct? pressure?
                                        pocket?
                                        throw-ball?
                                        eligible-receiver?)
  (or (and pressure? pocket? (not throw-ball?))
      (and pressure? pocket? throw-ball? (not eligible-receiver?))))

;; tests:
(check-expect (intentional-grounding-correct? #t #t #t #t) #f)
(check-expect (intentional-grounding-correct? #t #t #t #f) #t)
(check-expect (intentional-grounding-correct? #t #t #f #t) #t)
(check-expect (intentional-grounding-correct? #t #f #t #t) #f)
(check-expect (intentional-grounding-correct? #f #t #t #t) #f)
(check-expect (intentional-grounding-correct? #t #t #f #f) #t)
(check-expect (intentional-grounding-correct? #t #f #t #f) #f)
(check-expect (intentional-grounding-correct? #t #f #f #t) #f)
(check-expect (intentional-grounding-correct? #f #f #t #t) #f)
(check-expect (intentional-grounding-correct? #f #t #f #t) #f)
(check-expect (intentional-grounding-correct? #f #t #t #f) #f)
(check-expect (intentional-grounding-correct? #t #f #f #f) #f)
(check-expect (intentional-grounding-correct? #f #t #f #f) #f)
(check-expect (intentional-grounding-correct? #f #f #t #f) #f)
(check-expect (intentional-grounding-correct? #f #f #f #t) #f)
(check-expect (intentional-grounding-correct? #f #f #f #f) #f)

;-----------------------------------------------------------------

;; 3c)

;; (intentional-grounding-penalty pressure?
;;                                pocket?
;;                                throw-ball?
;;                                eligible-receiver?
;;                                endzone?)
;; determines the type of penalty that should be given given whether
;; there was pressure, whether it was inside the pocket, whether the
;; ball was thrown, whether the receiver was eligible, and whether it
;; occurred in the endzone.
;; intentional-grounding-penalty: Bool Bool Bool Bool Bool -> Sym
;; example:
(check-expect (intentional-grounding-penalty #t #t #t #f #t) 'Safety)

(define (intentional-grounding-penalty pressure?
                                       pocket?
                                       throw-ball?
                                       eligible-receiver?
                                       endzone?)
  (cond
    [(and (intentional-grounding-correct? pressure?
                                         pocket?
                                         throw-ball?
                                         eligible-receiver?)
          endzone?) 'Safety]
    [(and (intentional-grounding-correct? pressure?
                                         pocket?
                                         throw-ball?
                                         eligible-receiver?)
          (not endzone?)) '10yds]
    [else 'None]))

;; tests:
(check-expect (intentional-grounding-penalty #t #t #t #t #t) 'None)
(check-expect (intentional-grounding-penalty #t #t #t #f #t) 'Safety)
(check-expect (intentional-grounding-penalty #t #t #f #t #t) 'Safety)
(check-expect (intentional-grounding-penalty #t #f #t #t #t) 'None)
(check-expect (intentional-grounding-penalty #f #t #t #t #t) 'None)
(check-expect (intentional-grounding-penalty #t #t #f #f #t) 'Safety)
(check-expect (intentional-grounding-penalty #t #f #t #f #t) 'None)
(check-expect (intentional-grounding-penalty #t #f #f #t #t) 'None)
(check-expect (intentional-grounding-penalty #f #f #t #t #t) 'None)
(check-expect (intentional-grounding-penalty #f #t #f #t #t) 'None)
(check-expect (intentional-grounding-penalty #f #t #t #f #t) 'None)
(check-expect (intentional-grounding-penalty #t #f #f #f #t) 'None)
(check-expect (intentional-grounding-penalty #f #t #f #f #t) 'None)
(check-expect (intentional-grounding-penalty #f #f #t #f #t) 'None)
(check-expect (intentional-grounding-penalty #f #f #f #t #t) 'None)
(check-expect (intentional-grounding-penalty #f #f #f #f #t) 'None)
(check-expect (intentional-grounding-penalty #t #t #t #t #f) 'None)
(check-expect (intentional-grounding-penalty #t #t #t #f #f) '10yds)
(check-expect (intentional-grounding-penalty #t #t #f #t #f) '10yds)
(check-expect (intentional-grounding-penalty #t #f #t #t #f) 'None)
(check-expect (intentional-grounding-penalty #f #t #t #t #f) 'None)
(check-expect (intentional-grounding-penalty #t #t #f #f #f) '10yds)
(check-expect (intentional-grounding-penalty #t #f #t #f #f) 'None)
(check-expect (intentional-grounding-penalty #t #f #f #t #f) 'None)
(check-expect (intentional-grounding-penalty #f #f #t #t #f) 'None)
(check-expect (intentional-grounding-penalty #f #t #f #t #f) 'None)
(check-expect (intentional-grounding-penalty #f #t #t #f #f) 'None)
(check-expect (intentional-grounding-penalty #t #f #f #f #f) 'None)
(check-expect (intentional-grounding-penalty #f #t #f #f #f) 'None)
(check-expect (intentional-grounding-penalty #f #f #t #f #f) 'None)
(check-expect (intentional-grounding-penalty #f #f #f #t #f) 'None)
(check-expect (intentional-grounding-penalty #f #f #f #f #f) 'None)
    
                                            

  