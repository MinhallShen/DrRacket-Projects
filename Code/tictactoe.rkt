;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tictactoe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 5 Question 4
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; constants
(define grid1
  '((_ _ _)
    (_ _ _)
    (_ _ _)))

(define grid2
  '((X O O O _)
    (X X O _ _)
    (_ _ _ _ _)
    (_ _ _ _ _)
    (_ _ _ _ X)))

(define grid3
  '((X)))

;; a)
;; (count-X-in-row row) counts the number of 'Xs in a row of T3Grid
;; count-X-in-row: (listof (anyof 'X 'O '_)) -> Nat
;; requires: list length is odd
;; examples:
(check-expect (count-X-in-row '(X O X _)) 2)
(check-expect (count-X-in-row '()) 0)
(check-expect (count-X-in-row '(O O O)) 0)

(define (count-X-in-row row)
  (cond [(empty? row) 0]
        [else (+ (cond
                   [(symbol=? (first row) 'X) 1]
                   [else 0])
                 (count-X-in-row (rest row)))]))

;; (count-O-in-row row) counts the number of 'Os in a list
;; count-O-in-row: (listof (anyof 'X 'O '_)) -> Nat
;; examples:
(check-expect (count-O-in-row '(X O X _)) 1)
(check-expect (count-O-in-row '()) 0)
(check-expect (count-O-in-row '(X X X)) 0)

(define (count-O-in-row row)
  (cond [(empty? row) 0]
        [else (+ (cond
                   [(symbol=? (first row) 'O) 1]
                   [else 0])
                 (count-O-in-row (rest row)))]))

;; (count-X-in-grid grid) consumes a T3Grid and produces the number of 'Xs in
;;   grid.
;; count-X-in-grid: T3Grid -> Nat
;; examples:
(check-expect (count-X-in-grid '(())) 0)
(check-expect (count-X-in-grid grid1) 0)
(check-expect (count-X-in-grid grid2) 4)

(define (count-X-in-grid grid)
  (cond [(empty? grid) 0]
        [else (+ (count-X-in-row (first grid))
                 (count-X-in-grid (rest grid)))]))

;; (count-O-in-grid grid) consumes a T3Grid and produces the number of 'Os in
;;   grid.
;; count-O-in-grid: T3Grid -> Nat
;; examples:
(check-expect (count-O-in-grid '(())) 0)
(check-expect (count-O-in-grid grid1) 0)
(check-expect (count-O-in-grid grid2) 4)

(define (count-O-in-grid grid)
  (cond [(empty? grid) 0]
        [else (+ (count-O-in-row (first grid))
                 (count-O-in-grid (rest grid)))]))

;; (whose-turn grid) determines which player, 'X or 'O, has the next turn given
;;   T3Grid
;; whose-turn grid: T3Grid -> Sym
;; requires: grid must contain only 'X, 'O, and '_
;; examples:
(check-expect (whose-turn grid1) 'X)
(check-expect (whose-turn '(())) 'X)

(define (whose-turn grid)
  (cond [(empty? grid) 'X]
        [else (cond
                [(= (count-X-in-grid grid) (count-O-in-grid grid)) 'X]
                [else 'O])]))

;; tests:
(check-expect (whose-turn grid1) 'X)
(check-expect (whose-turn grid2) 'X)
(check-expect (whose-turn grid3) 'O)

;------------------------------------------------------------------------------                      

;; b)
;; (find-column-index row column-index) consumes a list and a column index and
;;   returns a value at the given index in the list.
;; find-column-index: (listof Sym) Nat -> Sym
;; requires: column-index < (length row), row must only contain ('X, 'O or '_)
;; examples:
(check-expect (find-column-index '(X O _) 0) 'X)
(check-expect (find-column-index '(_ _ O) 2) 'O)             

(define (find-column-index row column-index)
  (list-ref row column-index))

;; (find-row-index grid row-index) consumes a T3Grid and a row-index and
;;   a list at the given row number in the T3Grid.
;; find-row-index: T3Grid Nat -> (listof (anyof 'X 'O '_))
;; requires: row-index < (length grid), list length is odd
;; examples:
(check-expect (find-row-index grid2 0) '(X O O O _))
(check-expect (find-row-index grid2 2) '(_ _ _ _ _))

(define (find-row-index grid row-index)
  (list-ref grid row-index))

;; (grid-ref grid row column) consumes a T3Grid, row-index row and column-index
;;   column and produces the symbol at that location.
;; grid-ref: T3grid Nat Nat -> Sym
;; examples:
(check-expect (grid-ref grid2 1 2) 'O)
(check-expect (grid-ref grid2 0 0) 'X)

(define (grid-ref grid row column)
  (find-column-index (find-row-index grid row) column))

;; tests:
(check-expect (grid-ref grid2 1 2) 'O)
(check-expect (grid-ref grid2 0 0) 'X)
(check-expect (grid-ref grid3 0 0) 'X)
(check-expect (grid-ref grid1 2 2) '_)
(check-expect (grid-ref grid2 4 4) 'X)

;------------------------------------------------------------------------------

;; c)
;; (get-column grid column-num) consumes a T3Grid and a column number and
;;   and produces a list of the symbols in that column.
;; get-column: T3Grid Nat -> (listof (anyof 'X 'O '_))
;; requires: list length is odd
;; examples:
(check-expect (get-column grid1 0) '(_ _ _))
(check-expect (get-column grid2 1) '(O X _ _ _))
(check-expect (get-column grid3 0) '(X))

(define (get-column grid column-num)
  (cond [(empty? grid) empty]
        [else (cons (list-ref (first grid) column-num)
                    (get-column (rest grid) column-num))]))

;; tests:
(check-expect (get-column grid1 0) '(_ _ _))
(check-expect (get-column grid2 1) '(O X _ _ _))
(check-expect (get-column grid3 0) '(X))

;------------------------------------------------------------------------------

;; d)

;; (num-empty spaces row) counts the number of blank spaces '_ in a row of
;;   T3Grid
;; num-empty: (listof (anyof 'X 'O '_)) -> Nat
;; requires: list length is odd
;; examples:
(check-expect (num-empty-spaces '(X O _)) 1)
(check-expect (num-empty-spaces '(_ _ O)) 2)

(define (num-empty-spaces row)
  (cond [(empty? row) 0]
        [(symbol=? (first row) '_)
         (+ (num-empty-spaces (rest row)) 1)]
        [else (num-empty-spaces (rest row))]))

;; (valid-sym? lst player) determines whether a row or column contains only
;;   blank spaces '_ and the player
;; valid-sym?: (listof (anyof 'X 'O '_)) Sym -> Bool
;; requires: list length is odd, player must be (anyof 'X 'O '_)
;; examples:
(check-expect (valid-sym? '(X X X X _) 'X) true)
(check-expect (valid-sym? '(X X X X _) 'O) false)
(check-expect (valid-sym? '(X _ O) 'O) false)
(check-expect (valid-sym? '(_ _ _) 'X) true)

(define (valid-sym? lst player)
  (cond [(empty? lst) true]
        [(symbol=? (first lst) player) (valid-sym? (rest lst) player)]
        [(symbol=? '_ (first lst)) (valid-sym? (rest lst) player)]
        [else false]))

;; (will-win? grid row column player) consumers a T3Grid, row-index, column
;;   index and a player and determines if the player would win at that position.
;; will-win?: T3Grid Nat Nat Sym -> Bool
;; requires: there must be no row or column with the same player
;; examples:
(check-expect (will-win? grid1 0 0 'X) false)
(check-expect (will-win? '((X X _)
                           (O X O)
                           (O _ _))
                         0 2 'X) true)

(define (will-win? grid row column player)
  (or (and (= (num-empty-spaces (list-ref grid row)) 1)
           (valid-sym? (list-ref grid row) player))
      (and (= (num-empty-spaces (get-column grid column)) 1)
           (valid-sym? (get-column grid column) player))))

;; tests:
(check-expect (will-win? grid1 0 0 'X) false)
(check-expect (will-win? '((X X _)
                           (O X O)
                           (O _ _))
                         0 2 'X) true)
(check-expect (will-win? '((X X _)
                           (O X O)
                           (O _ _))
                         1 2 'O) false)
(check-expect (will-win? '((X X X)
                           (O X O)
                           (O _ _))
                         0 2 'X) false)
(check-expect (will-win? '((_ _ _)
                           (O X O)
                           (O _ _))
                         0 2 'X) false)
(check-expect (will-win? '((_ X _)
                           (O X O)
                           (_ _ _))
                         2 1 'X) true)
(check-expect (will-win? '((X X O)
                           (O X O)
                           (X _ _))
                         2 2 'O) true)








         
















  


  