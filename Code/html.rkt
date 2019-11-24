;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname html) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 8 Question 4
;; ***************************************************
;;
;------------------------------------------------------------------------------

(define html-example '(html (head (title "CS135"))
                            (body (h1 "Welcome")
                                  "More text...")))
(define just-text "Hello, world!")
(define short-example '(p (h1 "Heading") "Text"))



;; A

;; (html->string hi) consumes a HTML-Item and produces the equivalent HTML text.
;; html->string: HI -> Str
;; examples:
(check-expect (html->string "text") "text")
(check-expect (html->string short-example)
              "<p><h1>Heading</h1>Text</p>")
(check-expect (html->string '(hr)) "<hr></hr>")

(define (html->string hi)
  (local
    [(define (make-start-tag sym)
       (string-append "<" (symbol->string sym) ">"))
     (define (make-end-tag sym)
       (string-append "</" (symbol->string sym) ">"))]
    (cond
      [(string? hi) hi]
      [(empty? hi) ""]
      [(symbol? (first hi))
       (string-append (make-start-tag (first hi))
                      (html->string (rest hi))
                      (make-end-tag (first hi)))]
      [else
       (string-append (html->string (first hi))
                      (html->string (rest hi)))])))

;; tests:
(check-expect (html->string "text") "text")
(check-expect (html->string short-example)
              "<p><h1>Heading</h1>Text</p>")
(check-expect (html->string '(hr)) "<hr></hr>")
(check-expect (html->string html-example)
              "<html><head><title>CS135</title></head><body><h1>Welcome</h1>More text...</body></html>")

;; B

;; (remove-tag hi sym) consumes a HTML-Item and Symbol and removes all
;;   occurrences of that tag labelled with sym.
;; remove-tag: HI Sym -> HI
;; examples:
(check-expect (remove-tag html-example 'b) html-example)
(check-expect (remove-tag '(p "Hello, " (b "World") "!") 'b)
              '(p "Hello, " "World" "!"))
(check-expect (remove-tag '(p "Hello, " (b "World") "!") 'p)
              '("Hello, " (b "World") "!"))

(define (remove-tag hi sym)
  (cond
    [(empty? hi) empty]
    [(string? (first hi))
     (cons (first hi) (remove-tag (rest hi) sym))]
    [(and (list? (first hi))
          (symbol=? (first (first hi)) sym))
     (append (rest (first hi)) (remove-tag (rest hi) sym))]
    [(and (list? (first hi))
          (not (symbol=? (first (first hi)) sym)))
     (cons (first hi) (remove-tag (rest hi) sym))]
    [(symbol=? sym (first hi))
     (remove sym (remove-tag (rest hi) sym))]
    [(not (symbol=? (first hi) sym))
     (cons (first hi) (remove-tag (rest hi) sym))]))

;; tests:
(check-expect (remove-tag html-example 'b) html-example)
(check-expect (remove-tag '(p "Hello, " (b "World") "!") 'b)
              '(p "Hello, " "World" "!"))
(check-expect (remove-tag '(p "Hello, " (b "World") "!") 'p)
              '("Hello, " (b "World") "!"))
(check-expect (remove-tag '(p "Hello, " (p "World") "!") 'p)
              '("Hello, " "World" "!"))

;; C

;; (bad-tags? hi) consumes an HTML-Item and produces true if one of the two
;;   rules are violated and false otherwise.
;; bad-tags?: HI -> Bool
;; examples:
(check-expect (bad-tags? html-example) false)
(check-expect (bad-tags? '(body (li "Q1") "text")) true)

(define (bad-tags? hi)
  (local
    [;; (violate-rule1? hi) consumes a HTML-Item and returns true if there
     ;;   exists an 'li whos parents are not 'ol or 'ul.
     ;; violate-rule1: HI -> Bool
     (define (violate-rule1? hi)
       (cond
         [(empty? hi) false]
         [(string? (first hi)) (violate-rule1? (rest hi))]
         [(list? (first hi)) (or (violate-rule1? (first hi))
                                 (violate-rule1? (rest hi)))]
         [(or (symbol=? (first hi) 'ol)
              (symbol=? (first hi) 'ul))
          (violate-rule1? (rest hi))]
         [(symbol? (first hi))
          (cond
            [(empty? (rest hi)) false]
            [(string? (first (rest hi))) (violate-rule1? (rest (rest hi)))]
            [(and (list? (first (rest hi)))
                  (symbol=? (first (first (rest hi))) 'li)) true]
            [else (violate-rule1? (first (rest hi)))])]))
     ;; (violate-rule2 hi) consumes a HTML-Item and returns true if there
     ;;   exists an 'hr with children.
     ;; violate-rule2: HI -> Bool
     (define (violate-rule2? hi)
       (cond
         [(empty? hi) false]
         [(string? (first hi)) (violate-rule2? (rest hi))]
         [(list? (first hi)) (or (violate-rule2? (first hi))
                                 (violate-rule2? (rest hi)))]
         [(and (symbol=? (first hi) 'hr)
               (empty? (rest hi))) false]
         [(and (symbol=? (first hi) 'hr)
               (not (empty? (rest hi)))) true]
         [else (violate-rule2? (rest hi))]))]
    (or (violate-rule1? hi) (violate-rule2? hi))))

;; tests:
(check-expect (bad-tags? html-example) false)
(check-expect (bad-tags? '(body (hr "hello"))) true)
(check-expect (bad-tags? '(body (li "Q1") "text")) true)
(check-expect (bad-tags? '(body (ul (hr) "hello"))) false)
(check-expect (bad-tags? '(body (ul (li "string") (li "string")) "st")) false)
(check-expect (bad-tags? '(ok "a" (ul (li "text") (li "asfa")) "a")) false)






    
     
     
               
     
     
    
    