;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname not-catan) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 6 Question 1
;; ***************************************************
;;
;------------------------------------------------------------------------------

(define-struct inventory (blocks wood sheep wheat rocks))
;; An Inventory is a (make-inventory Nat Nat Nat Nat Nat)
;; requires: wheat >= sheep
;; wheat >= 4

(define-struct cost (blocks wood sheep wheat rocks))
;; A Cost is a (make-cost Nat Nat Nat Nat Nat)


;; a)
;; inventory-template: Inventory -> Any

(define (inventory-template resources)
  (... (inventory-blocks resources) ...
       (inventory-wood resources) ...
       (inventory-sheep resources) ...
       (inventory-wheat resources) ...
       (inventory-rocks resources) ...))

;------------------------------------------------------------------------------

;; b)
;; (valid-inventory? inv) consumes an inventory and produces true if the
;;   inventory is a valid inventory.
;; valid-inventory?: Inventory -> Bool
;; examples:
(check-expect (valid-inventory? (make-inventory 3 2 4 5 1)) true)
(check-expect (valid-inventory? (make-inventory 3 2 2 3 1)) false)

(define (valid-inventory? inv)
  (cond [(not (inventory? inv)) false]
        [(and (>= (inventory-wheat inv) (inventory-sheep inv))
              (>= (inventory-blocks inv) 0)
              (>= (inventory-wood inv) 0)
              (>= (inventory-sheep inv) 0)
              (>= (inventory-wheat inv) 4)
              (>= (inventory-rocks inv) 0))
              true]
        [else false]))

;; tests:
(check-expect (valid-inventory? (make-inventory 3 2 4 5 1)) true)
(check-expect (valid-inventory? (make-inventory 3 2 2 3 1)) false)
(check-expect (valid-inventory? '(1 2 3 4 5)) false)
(check-expect (valid-inventory? '(hi yes ok 4 5)) false)
(check-expect (valid-inventory? '("1" "2" "3" "4" "5")) false)

;------------------------------------------------------------------------------

;; c)
;; (new-inv inv cost) consumes an inventory and a cost and produces a new
;;   inventory, subtracting the cost of each resource from the inventory.
;; new-inv: Inventory Cost -> Inventory
;; examples:
(check-expect
 (new-inv (make-inventory 3 2 4 5 1) (make-cost 0 2 0 1 0))
 (make-inventory 3 0 4 4 1))
(check-expect
 (new-inv (make-inventory 3 2 5 5 1) (make-cost 0 2 0 1 2))
 (make-inventory 3 0 5 4 -1))
 
(define (new-inv inv cost)
  (make-inventory (- (inventory-blocks inv) (cost-blocks cost))
                  (- (inventory-wood inv) (cost-wood cost))
                  (- (inventory-sheep inv) (cost-sheep cost))
                  (- (inventory-wheat inv) (cost-wheat cost))
                  (- (inventory-rocks inv) (cost-rocks cost))))

;; (affordable? inv cost) consumes an inventory and a cost and produces true if
;;   the cost is covered by the inventory and if the resulting inventory remains
;;   a valid one and false toherwise.
;; affordable?: Inventory Cost -> Bool
;; examples:
(check-expect
 (affordable? (make-inventory 3 2 4 5 1) (make-cost 0 2 0 1 0)) true)
(check-expect
 (affordable? (make-inventory 3 2 5 5 1) (make-cost 0 2 0 1 0)) false)

(define (affordable? inv cost)
  (cond [(valid-inventory? (new-inv inv cost)) true]
        [else false]))

;; tests:
(check-expect
 (affordable? (make-inventory 3 2 4 5 1) (make-cost 0 2 0 1 0)) true)
(check-expect
 (affordable? (make-inventory 3 2 5 5 1) (make-cost 0 2 0 1 0)) false)
(check-expect
(affordable? (make-inventory 3 2 4 5 1) (make-cost 0 3 0 1 0)) false)

;------------------------------------------------------------------------------

;; d)
;; (thief inv) consumes an inventory and subtracts one unit of each resource
;;   or zero units of each resource if that is not possible.
;; thief: Inventory -> Inventory
;; examples:
(check-expect
 (thief (make-inventory 3 2 4 5 0)) (make-inventory 2 1 3 4 0))
(check-expect
 (thief (make-inventory 3 2 4 4 0)) (make-inventory 2 1 3 4 0))

(define (thief inv)
  (make-inventory
   (cond [(>= (inventory-blocks inv) 1) (sub1 (inventory-blocks inv))]
         [else 0])
   (cond [(>= (inventory-wood inv) 1) (sub1 (inventory-wood inv))]
         [else 0])
   (cond [(>= (inventory-sheep inv) 1) (sub1 (inventory-sheep inv))]
         [else 0])
   (cond [(>= (inventory-wheat inv) 5) (sub1 (inventory-wheat inv))]
         [else 4])
   (cond [(>= (inventory-rocks inv) 1) (sub1 (inventory-rocks inv))]
         [else 0])))

;; tests:
(check-expect
 (thief (make-inventory 3 2 4 5 0)) (make-inventory 2 1 3 4 0))
(check-expect
 (thief (make-inventory 3 2 4 4 0)) (make-inventory 2 1 3 4 0))
(check-expect
 (thief (make-inventory 3 2 4 5 1)) (make-inventory 2 1 3 4 0))
(check-expect
 (thief (make-inventory 0 2 4 5 1)) (make-inventory 0 1 3 4 0))
(check-expect
 (thief (make-inventory 3 0 4 5 1)) (make-inventory 2 0 3 4 0))
(check-expect
 (thief (make-inventory 3 2 0 5 1)) (make-inventory 2 1 0 4 0))
(check-expect
 (thief (make-inventory 0 0 0 4 0)) (make-inventory 0 0 0 4 0))
(check-expect
 (thief (make-inventory 3 2 4 5 2)) (make-inventory 2 1 3 4 1))
   
    


