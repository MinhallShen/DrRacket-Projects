;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname books) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 5 Question 3
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; constants:
(define my-bookshelf '(("The Colour of Magic" "Pratchett, Terry")
                       ("Mostly Harmless" "Adams, Douglas")
                       ("Pyramids" "Pratchett, Terry")
                       ("A Brief History of Time" "Hawking, Stephen")
                       ("Last Chance to See" "Adams, Douglas")
                       ("Good Omens" "Pratchett, Terry")))

(define my-bookshelf-sorted '(("Last Chance to See" "Adams, Douglas")
                              ("Mostly Harmless" "Adams, Douglas")
                              ("A Brief History of Time" "Hawking, Stephen")
                              ("Good Omens" "Pratchett, Terry")
                              ("Pyramids" "Pratchett, Terry")
                              ("The Colour of Magic" "Pratchett, Terry")))

(define discworld-books '(("The Colour of Magic" "Pratchett, Terry")
                          ("Pyramids" "Pratchett, Terry")
                          ("Good Omens" "Pratchett, Terry")))

(define my-index '(("Pratchett, Terry"
                    "The Colour of Magic"
                    "Pyramids"
                    "Good Omens")
                   ("Hawking, Stephen" "A Brief History of Time")
                   ("Adams, Douglas")))

;; a)
;; A Book is a (listof Str Str)
;; book-template: Book -> Any
(define (book-template book)
  (cond [(empty? book) ...]
        [else (... (first book) ...
                   ... (book-template (rest book)) ...)]))

;; A (listof Book) is one of:
;; * empty
;; * (listof (listof Str Str))
;; listof-book-template: (listof Book) -> Any

(define (listof-book-template lob)
  (cond [(empty? lob) ...]
        [else (... (book-template (first lob))  ...
                   ... (listof-book-template (rest lob)) ...)]))
                  

;------------------------------------------------------------------------------

;; b)


;; (insert-books my-book my-bookshelf) inserts a book my-book into the
;;   lexicographically sorted my-bookshelf so that the resulting list of books
;;   is also sorted.
;; insert-books: Book (listof Book) -> (listof Book)
;; requires: my-bookshelf is sorted lexicographically by author's last name
;; examples:
(check-expect (insert-books '("Harry Potter" "Rowling, JK") my-bookshelf-sorted)
              '(("Last Chance to See" "Adams, Douglas")
                ("Mostly Harmless" "Adams, Douglas")
                ("A Brief History of Time" "Hawking, Stephen")
                ("Good Omens" "Pratchett, Terry")
                ("Pyramids" "Pratchett, Terry")
                ("The Colour of Magic" "Pratchett, Terry")
                ("Harry Potter" "Rowling, JK")))
(check-expect (insert-books '("AAA" "Adams, Douglas") my-bookshelf-sorted)
              '(("AAA" "Adams, Douglas")
                ("Last Chance to See" "Adams, Douglas")
                ("Mostly Harmless" "Adams, Douglas")
                ("A Brief History of Time" "Hawking, Stephen")
                ("Good Omens" "Pratchett, Terry")
                ("Pyramids" "Pratchett, Terry")
                ("The Colour of Magic" "Pratchett, Terry")))

(define (insert-books my-book bookshelf)
  (cond [(empty? bookshelf) (cons my-book empty)]
        [(string<? (second my-book) (second (first bookshelf)))
         (cons my-book bookshelf)]
        [(string=? (second my-book) (second (first bookshelf)))
         (cond [(string<=? (first my-book) (first (first bookshelf)))
                (cons my-book bookshelf)]
               [else (cons (first bookshelf) 
                           (insert-books my-book (rest bookshelf)))])]
        [else (cons (first bookshelf)
                    (insert-books my-book (rest bookshelf)))]))         

;; (sort-books my-bookshelf) consumes a list of books and organizes them
;;   lexicographically by the author's name (in the form of lastname, firstname)
;;   and books with the same author will be sorted lexicographically by book
;;   title.
;; sort-books: (listof Book) -> (listof Book)
;; example:
(check-expect (sort-books my-bookshelf)
              my-bookshelf-sorted)

(define (sort-books bookshelf)
  (cond [(empty? bookshelf) empty]
        [else (insert-books (first bookshelf)
                            (sort-books (rest bookshelf)))]))

;; tests:
(check-expect (sort-books empty) empty)
(check-expect (sort-books my-bookshelf)
              my-bookshelf-sorted)

;------------------------------------------------------------------------------

;; c)
;; (books-by-author bookshelf author) consumes a list of books and an author and
;;   produces a list of books written by the author.
;; books-by-author: (listof Book) Str -> (listof Book)
;; examples:
(check-expect (books-by-author my-bookshelf "Pratchett, Terry")
              discworld-books)
(check-expect (books-by-author my-bookshelf "King, Stephen")
              empty)

(define (books-by-author bookshelf author)
  (cond [(empty? bookshelf) empty]
        [else (cond [(string=? (second (first bookshelf)) author)
                     (cons (first bookshelf)
                           (books-by-author (rest bookshelf) author))]
                    [else (books-by-author (rest bookshelf) author)])]))

;; tests:
(check-expect (books-by-author my-bookshelf "Pratchett, Terry")
              discworld-books)
(check-expect (books-by-author my-bookshelf "Riordan, Rick")
              empty)
(check-expect (books-by-author my-bookshelf "Adams, Douglas")
              '(("Mostly Harmless" "Adams, Douglas")
                ("Last Chance to See" "Adams, Douglas")))
              
;------------------------------------------------------------------------------

;; d)
;; (author-in-index? author-index author) returns true if an author is within an
;;   an author index and false otherwise.
;; author-in-index?: AuthorIndex Str -> Bool
;; examples:
(check-expect (author-in-index? empty "King, Stephen") false)
(check-expect (author-in-index? my-index "Adams, Douglas") true)
(check-expect (author-in-index? my-index "Rowling, JK") false)

(define (author-in-index? author-index author)
  (cond [(empty? author-index) false]
        [(string=? author (first (first author-index))) true]
        [else (author-in-index? (rest author-index) author)]))

;; (find-author author-index author) consumes an author index and an author and
;;   returns the list of an author and his/her books within the author index.
;; find-author: AuthorIndex Str -> (listof Str)
;; requires: author must be in author-index
;; examples:
(check-expect (find-author my-index "Pratchett, Terry")
              (list "Pratchett, Terry" "The Colour of Magic"
                    "Pyramids"
                    "Good Omens"))
(check-expect (find-author my-index "Adams, Douglas")
              (list "Adams, Douglas"))
(define (find-author author-index author)
  (cond [(string=? (first (first author-index)) author)
         (first author-index)]
        [else (find-author (rest author-index) author)]))

;; (book-in-element-of-index?) consumes a list of an author and his/her books
;;   within the author index and a title of a book and produces true if the book
;;   is within the list and false if it is not.
;; book-in-element-of-index?: (listof Str) Str -> Bool
;; examples:
(check-expect (book-in-element-of-index? (first my-index) "Pyramids") true)
(check-expect (book-in-element-of-index? (first my-index) "Harry Potter") false)
(check-expect (book-in-element-of-index? empty "a") false)

(define (book-in-element-of-index? element-of-index title)
  (cond [(empty? element-of-index) false]
        [(= (length element-of-index) 1) false]
        [(string=? (second element-of-index) title) true]
        [else (book-in-element-of-index? (rest element-of-index) title)]))

;; (book-by-author? author-index author title) consumes and author index, an
;;   author and the title of a book and returns true if the author index
;;   contains a book with the same author and title.
;; book-by-author?: AuthorIndex Str Str -> Bool
;; examples:
(check-expect (book-by-author? my-index
                               "Pratchett, Terry"
                               "The Colour of Magic") true)
(check-expect (book-by-author? my-index "King, Stephen" "It") false)

(define (book-by-author? author-index author title)
  (cond [(empty? author-index) false]
        [(author-in-index? author-index author)
         (cond [(book-in-element-of-index? (find-author author-index author)
                                           title) true]
               [else false])]
         [else false]))

;; tests:
(check-expect (book-by-author? empty "." ".") false)
(check-expect (book-by-author? my-index
                               "Adams, Douglas"
                               "Mostly Harmless") false)
(check-expect (book-by-author? my-index
                               "Pratchett, Terry"
                               "The Colour of Magic") true)
(check-expect (book-by-author? my-index
                               "Hawking, Stephen"
                               "A Brief History of Time") true)
(check-expect (book-by-author? my-index
                               "Pratchett, Terry"
                               "A Brief History of Time") false)
        
;------------------------------------------------------------------------------

;; e)
;; (list-of-books-by-author bookshelf author) consumes a list of books and an
;;   author's name and produces a list of the books by the author in the order
;;   they were consumed.
;; list-of-books-by-author: (listof Book) Str -> (listof Str)
;; examples:
(check-expect (list-of-books-by-author empty "Author, Random") empty)
(check-expect (list-of-books-by-author my-bookshelf "Pratchett, Terry")
              (list "The Colour of Magic" "Pyramids" "Good Omens"))

(define (list-of-books-by-author bookshelf author)
  (cond [(empty? bookshelf) empty]
        [(string=? author (second (first bookshelf)))
         (cons (first (first bookshelf))
               (list-of-books-by-author (rest bookshelf) author))]
        [else (list-of-books-by-author (rest bookshelf) author)]))

;; (make-element-of-index bookshelf author) consumes a list of books and an
;;   author's name and produces a list containing the author's name and his/her
;;   books in the order they were consumed.
;; make-element-of-index: (listof Book) Str -> (listof Str)
;; tests:
(check-expect (make-element-of-index my-bookshelf "Pratchett, Terry")
              (list "Pratchett, Terry"
                    "The Colour of Magic"
                    "Pyramids"
                    "Good Omens"))
(define (make-element-of-index bookshelf author)
  (cons author (list-of-books-by-author bookshelf author)))

;; (build-author-index consumes a list of books and a list of unique authors and
;;   produces an AuthorIndex, maintaining the order of books and authors
;;   consumed.
;; build-author-index: (listof Book) (listof Str) -> AuthorIndex
;; requires: authors in author-list must be unique
;; tests:
(check-expect (build-author-index my-bookshelf '("Adams, Douglas"
                                                 "Hawking, Stephen"))
              '(("Adams, Douglas" "Mostly Harmless" "Last Chance to See")
                ("Hawking, Stephen" "A Brief History of Time")))
(check-expect (build-author-index my-bookshelf empty) empty)

(define (build-author-index bookshelf author-list)
  (cond
    [(empty? bookshelf) empty]
    [(empty? author-list) empty]
    [else
     (cons (make-element-of-index bookshelf (first author-list))
           (build-author-index bookshelf (rest author-list)))]))

;; tests:
(check-expect (build-author-index my-bookshelf '("Adams, Douglas"
                                                 "Hawking, Stephen"))
              '(("Adams, Douglas" "Mostly Harmless" "Last Chance to See")
                ("Hawking, Stephen" "A Brief History of Time")))
(check-expect (build-author-index my-bookshelf empty) empty)
(check-expect (build-author-index empty '("Author, Random")) empty)
(check-expect (build-author-index empty '()) empty)
(check-expect (build-author-index my-bookshelf '("Pratchett, Terry"))
              '(("Pratchett, Terry" "The Colour of Magic"
                                    "Pyramids"
                                    "Good Omens")))
(check-expect (build-author-index my-bookshelf '("Pratchett, Terry"
                                                 "Hawking, Stephen"
                                                 "Adams, Douglas"))
              '(("Pratchett, Terry" "The Colour of Magic"
                                    "Pyramids"
                                    "Good Omens")
                ("Hawking, Stephen" "A Brief History of Time")
                ("Adams, Douglas" "Mostly Harmless" "Last Chance to See")))
(check-expect (build-author-index my-bookshelf '("Pratchett, Terry"
                                                 "Adams, Douglas"
                                                 "Hawking, Stephen"))
              '(("Pratchett, Terry" "The Colour of Magic"
                                    "Pyramids"
                                    "Good Omens")
                ("Adams, Douglas" "Mostly Harmless" "Last Chance to See")
                ("Hawking, Stephen" "A Brief History of Time")))
(check-expect (build-author-index my-bookshelf '("Pratchett, Terry"
                                                 "Rowling, JK"))
              '(("Pratchett, Terry" "The Colour of Magic"
                                    "Pyramids"
                                    "Good Omens")
                ("Rowling, JK")))
(check-expect (build-author-index my-bookshelf '("Rowling, JK"))
              '(("Rowling, JK")))
(check-expect (build-author-index my-bookshelf '("Rowling, JK"
                                                 "Pratchett, Terry"))
              '(("Rowling, JK")
                ("Pratchett, Terry" "The Colour of Magic"
                                    "Pyramids"
                                    "Good Omens")))
              

                                                 






