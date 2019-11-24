;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname box-office) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 2 Question 2
;; ***************************************************
;;
;------------------------------------------------------------------------------
;; constants

(define profit-per-explosion 6)
(define profit-per-famous-actor 50)
(define marvel-profit 500)
(define dc-penalty -250)
(define short-name-profit 25)
(define the-penalty -50)

;------------------------------------------------------------------------------

;; (calc-name-length-profit movie-name) calculates the profit gained based on
;;the movie's name.
;; calc-name-length-profit: Str -> Int
;; examples:
(check-expect (calc-name-length-profit "The Big Elephant") the-penalty)

(define (calc-name-length-profit movie-name)
  (cond
    [(< (string-length movie-name) 3) short-name-profit]
    ;; if the movie name is less than 3 characters, then an error would occur
    ;;when trying to test if the first 3 letters
    ;; contains "The"
    [(and (< (string-length movie-name) 10)
          (string=? "The" (substring movie-name 0 3)))
     (+ the-penalty short-name-profit)]
    [(< (string-length movie-name) 10) short-name-profit]
    [(string=? "The" (substring movie-name 0 3)) the-penalty]
    [else 0]))

;; tests:
(check-expect (calc-name-length-profit "The Hulk")
              (+ the-penalty short-name-profit))
(check-expect (calc-name-length-profit "Up") short-name-profit)
(check-expect (calc-name-length-profit "The Incredibles") the-penalty)
(check-expect (calc-name-length-profit "Black Panther") 0)

;------------------------------------------------------------------------------

;; (calc-studio-name-profit studio-name) calculates the profit gained based on
;; the studio's name.
;; calc-studio-name-profit: Str -> Int
;; examples:
(check-expect (calc-studio-name-profit "Dreamworks") 0)

(define (calc-studio-name-profit studio-name)
  (cond
    [(string=? studio-name "DC") dc-penalty]
    [(string=? studio-name "Marvel") marvel-profit]
    [else 0]))

;; tests:
(check-expect (calc-studio-name-profit "DC") -250)
(check-expect (calc-studio-name-profit "Marvel") 500)
(check-expect (calc-studio-name-profit "Pixar") 0)

;------------------------------------------------------------------------------

;; (calc-num-famous-actors-profit num-of-famous-actors) calculates the profit
;; gained based on the number of famous actors in the movie.
;; calc-num-famous-actors-profit: Nat -> Num
;; example:
(check-expect (calc-num-famous-actors-profit 3) 150)

(define (calc-num-famous-actors-profit num-of-famous-actors)
  (* num-of-famous-actors profit-per-famous-actor))

;; tests:
(check-expect (calc-num-famous-actors-profit 0) 0)
(check-expect (calc-num-famous-actors-profit 5) 250)
  
;------------------------------------------------------------------------------

;; (calc-explosion-profit num-of-explosions) calculates how much profit
;; is made from the number of explosions in a movie given the number of
;; explosions
;; num-of-explosions: Nat -> Int
;; example:
(check-expect (calc-explosion-profit 4) 4)
(define (calc-explosion-profit num-of-explosions)
  (- (* profit-per-explosion num-of-explosions) 20))

;; tests:
(check-expect (calc-explosion-profit 0) -20)
(check-expect (calc-explosion-profit 3) -2)
(check-expect (calc-explosion-profit 5) 10)

;------------------------------------------------------------------------------

;; (box-office-profits movie-name studio-name num-of-famous-actors
;; num-of-explosions)
;; predicts the net amount of money a movie will make given the length of
;; its name, the name of the studio, the number of famous actors and the
;; number of explosions.
;; Requires: num-of-famous-actors, num-of-explosions >= 0
;; box-office-profits: Str Str Nat Nat -> Int
;; examples:
(check-expect (box-office-profits "Avengers: more endgames" "Marvel" 4 50) 980)
(check-expect (box-office-profits "Superman v Superman" "DC" 2 100) 430)
(check-expect (box-office-profits "The Slog" "New Line Cinema" 0 0) -45)

(define (box-office-profits movie-name studio-name num-of-famous-actors
                            num-of-explosions)
  (+ (calc-name-length-profit movie-name)
     (calc-studio-name-profit studio-name)
     (calc-num-famous-actors-profit num-of-famous-actors)
     (calc-explosion-profit num-of-explosions)))

;; tests:
(check-expect (box-office-profits "<10"
                                  "DC"
                                  1
                                  5) -165)
(check-expect (box-office-profits "Loooooooooong Title"
                                  "DC"
                                  4
                                  4) -46)
(check-expect (box-office-profits "The Long Title"
                                  "DC"
                                  0
                                  0) -320)
(check-expect (box-office-profits "The <10"
                                  "DC"
                                  0
                                  0) -295)
(check-expect (box-office-profits "<3"
                                  "DC"
                                  3
                                  4) -71)
(check-expect (box-office-profits "<10"
                                  "Marvel"
                                  2
                                  4) 629)
(check-expect (box-office-profits "Looooooooong Title"
                                  "Marvel"
                                  8
                                  8) 928)
(check-expect (box-office-profits "The Long Title"
                                  "Marvel"
                                  5
                                  4) 704)
(check-expect (box-office-profits "The <10"
                                  "Marvel"
                                  1
                                  4) 529)
(check-expect (box-office-profits "<10"
                                  "Marvel"
                                  2
                                  4) 629)
(check-expect (box-office-profits "<3"
                                  "Marvel"
                                  10
                                  10) 1065)
(check-expect (box-office-profits "<10"
                                  "Not Marvel or DC"
                                  6
                                  1) 311)
(check-expect (box-office-profits "Looooooooong Title"
                                  "Not Marvel or DC"
                                  3
                                  8) 178)
(check-expect (box-office-profits "The Long Title"
                                  "Not Marvel or DC"
                                  5
                                  4) 204)
(check-expect (box-office-profits "The <10"
                                  "Not Marvel or DC"
                                  0
                                  0) -45)
(check-expect (box-office-profits "<10"
                                  "Not Marvel or DC"
                                  0
                                  4) 29)
(check-expect (box-office-profits "<3"
                                  "Not Marvel or DC"
                                  2
                                  0) 105)


                                  

              