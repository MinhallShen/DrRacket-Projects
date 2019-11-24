;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tutorial-questions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (sum-num lon)
  (cond [(empty? lon) 0]
        [else (+ (first lon) (sum-num (rest lon)))]))

(define (factorial n)
  (cond
    [(= n 1) 1]
    [else (* n (factorial (sub1 n)))]))

(define (strings-equal? los)
  (cond
    [(empty? los) true]
    [(= 1 (length los)) true]
    [(string=? (first los) (second los)) (strings-equal? (rest los))]
    [else false]))

(define lonum (list (list 5) (list 4 3) (list 2 1)))


(define (list=? lst1 lst2)
  (cond [(and (empty? lst1) (empty? lst2))true]
        [(not (= (length lst1) (length lst2))) false]
        [(= (first lst1) (first lst2))
         (list=? (rest lst1) (rest lst2))]
        [else false]))

(define-struct card (suit value))
(define h-pts 5)
(define d-pts 4)
(define s-pts 0)
(define c-pts -5)

(define (card-score card)
  (cond
    [(symbol=? 'hearts (card-suit card))
     (+ (card-value card) h-pts)]
    [(symbol=? 'diamonds (card-suit card))
     (+ (card-value card) d-pts)]
    [(symbol=? 'spades (card-suit card))
     (+ (card-value card) s-pts)]
    [else (+ (card-value card) c-pts)]))

(check-expect (card-score (make-card 'hearts 13)) 18)

(define-struct hand (card1 card2 card3))

(define first-card (make-card 'hearts 3))
(define second-card (make-card 'spades 1))
(define third-card (make-card 'clubs 12))
(define my-hand (make-hand first-card second-card third-card))

(define (hand-score hand)
  (+ (cond
       [(> (card-score (hand-card1 hand)) 0) (card-score (hand-card1 hand))]
       [else 0])
     (cond
       [(> (card-score (hand-card2 hand)) 0) (card-score (hand-card2 hand))]
       [else 0])
     (cond
       [(> (card-score (hand-card3 hand)) 0) (card-score (hand-card3 hand))]
       [else 0])))

(define-struct piece (colour value))
;; Chess Piece Values:
(define pawn 1)
(define bishop 3)
(define knight 3)
(define rook 5)
(define queen 9)
;; Creating chess pieces
(define white-rook (make-piece 'white rook))
(define white-pawn (make-piece 'white pawn))
(define white-queen (make-piece 'white queen))
(define black-knight (make-piece 'black knight))
(define black-queen (make-piece 'black queen))
(define white-knight (make-piece 'white knight))

(define (add-black lop)
  (cond
    [(empty? lop) 0]
    [(symbol=? 'black (piece-colour (first lop)))
     (+ (piece-value (first lop)) (add-black (rest lop)))]
    [else (add-black (rest lop))]))

(define (add-white lop)
  (cond
    [(empty? lop) 0]
    [(symbol=? 'white (piece-colour (first lop)))
     (+ (piece-value (first lop)) (add-white (rest lop)))]
    [else (add-white (rest lop))]))

(define (who-won lop)
  (cond
    [(< (add-white lop) (add-black lop)) 'white]
    [(> (add-white lop) (add-black lop)) 'black]
    [else 'draw]))

(check-expect (who-won (list black-knight
white-queen
black-knight))
'black)

(check-expect (who-won (list black-knight black-queen white-queen)) 'white)
(check-expect (who-won (list black-knight white-knight)) 'draw)

(define (insert-at/list lst1 lst2 n)
  (cond
    [(empty? lst2) lst1]
    [(= n 0)
     (cons (first lst2) (insert-at/list lst1 (rest lst2) n))]
    [else (cons (first lst1) (insert-at/list (rest lst1) lst2 (sub1 n)))]))

(define (insert-at s1 s2 n)
  (list->string (insert-at/list (string->list s1) (string->list s2) n)))








       
