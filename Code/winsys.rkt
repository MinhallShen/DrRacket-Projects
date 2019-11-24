;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname winsys) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; An RTree (Rectangle Tree) is one of:
;; * Sym (a leaf) 
;; * XNode (a boundary node for an x value)
;; * YNode (a boundary node for a y value)

(define-struct xnode (val left right))
;; An XNode is a (make-xnode Nat RTree RTree)

(define-struct ynode (val below above))
;; A YNode is a (make-ynode Nat RTree RTree)


;; 
;; Q3a
;;

(define (rtree-template rtree)
  (cond
    [(symbol? rtree) ...]
    [(xnode? rtree) (... (xnode-template rtree) ...)]
    [(ynode? rtree) (... (ynode-template rtree) ...)]))

(define (xnode-template xnode)
  (... (xnode-val xnode) ...
       (rtree-template (xnode-left xnode)) ...
       (rtree-template (xnode-right xnode)) ...))

(define (ynode-template ynode)
  (... (ynode-val ynode) ...
       (rtree-template (ynode-left ynode)) ...
       (rtree-template (ynode-right ynode)) ...))

;;
;; Q3b
;;

;;
;;  Test cases
;;

;; testcase 1: a square
(define l1 (make-xnode  0  'None 'Content))
(define r1 (make-xnode 70   l1   'None))
(define b1 (make-ynode  0  'None  r1))
(define t1 (make-ynode 70   b1   'None))

;; testcase 2: a simple window
(define x60 (make-xnode 60 'Menu   'x))
(define y60 (make-ynode 60 'Content x60))
(define  l2 (make-xnode  0 'None    y60))
(define  r2 (make-xnode 70  l2     'None))
(define  b2 (make-ynode  0 'None    r2))
(define  t2 (make-ynode 70  b2     'None))

;; (rt-lookup rt pos) produces the symbol from the RTree that is associated 
;;   with the rectangle that contains pos
;; rt-lookup: RTree Posn -> Sym
;; examples:
(check-expect (rt-lookup 'Desktop (make-posn 2 2)) 'Desktop)
(check-expect (rt-lookup (make-xnode 5 'left 'right) (make-posn 2 2))'left)
(check-expect (rt-lookup (make-xnode 5 'left 'right) (make-posn 5 5))'right)
(check-expect (rt-lookup (make-ynode 5 'bottom 'top) (make-posn 2 2))'bottom)
(check-expect (rt-lookup (make-ynode 5 'bottom 'top) (make-posn 5 5))'top)

(define (rt-lookup rt pos)
  (cond
    [(symbol? rt) rt]
    [(xnode? rt)
     (cond
       [(< (posn-x pos) (xnode-val rt))
        (rt-lookup (xnode-left rt) pos)]
       [else (rt-lookup (xnode-right rt) pos)])]
    [(< (posn-y pos) (ynode-val rt))
     (rt-lookup (ynode-below rt) pos)]
    [else (rt-lookup (ynode-above rt) pos)]))

;; tests:
;; pos on bottom boundary
(check-expect (rt-lookup t1 (make-posn 0 35)) 'Content)

;; pos on left boundary
(check-expect (rt-lookup t1 (make-posn 35 0)) 'Content)

;; pos on top boundary
;; recall that rectangle only includes x < (xnode-val rt)
(check-expect (rt-lookup t1 (make-posn 70 35)) 'None)

;; pos on right boundary
;; recall that rectangle only includes y < (ynode-val rt)
(check-expect (rt-lookup t1 (make-posn 35 70)) 'None)
(check-expect (rt-lookup 'Desktop (make-posn 2 2)) 'Desktop)
(check-expect (rt-lookup (make-xnode 5 'left 'right) (make-posn 2 2))'left)
(check-expect (rt-lookup (make-xnode 5 'left 'right) (make-posn 5 5))'right)
(check-expect (rt-lookup (make-ynode 5 'bottom 'top) (make-posn 2 2))'bottom)
(check-expect (rt-lookup (make-ynode 5 'bottom 'top) (make-posn 5 5))'top)
(check-expect (rt-lookup t2 (make-posn 0 70)) 'None)
(check-expect (rt-lookup t2 (make-posn 0 69)) 'Menu)
(check-expect (rt-lookup t2 (make-posn 100 50)) 'None)
(check-expect (rt-lookup t2 (make-posn 70 10)) 'None)
(check-expect (rt-lookup t2 (make-posn 60 60)) 'x)
(check-expect (rt-lookup t2 (make-posn 59 59)) 'Content)



;;
;; Q3c
;;

;; (x-node-in-rt? rt) consumes an RTree and produces true if the RTree contains
;;   an xnode and false otherwise
;; x-node-in-rt?: RTree -> Bool
;; examples:
(check-expect (x-node-in-rt? t1) true)
(check-expect (x-node-in-rt? 'a) false)

(define (x-node-in-rt? rt)
  (cond
    [(symbol? rt) false]
    [(xnode? rt) true]
    [else (or (x-node-in-rt? (ynode-below rt))
              (x-node-in-rt? (ynode-above rt)))]))

;; (find-rt-max-x rt) consumes an RTree and produces the largest xnode value in
;;   the RTree and produces -1 if there is no xnode.
;; find-rt-max-x: RTree -> Num
;; examples:
(check-expect (find-rt-max-x (make-xnode 8 'a 'b)) 8)
(check-expect (find-rt-max-x (make-ynode 8 'a 'b)) -1)

(define (find-rt-max-x rt)
  (cond
    [(symbol? rt) -1]
    [(xnode? rt) (max (xnode-val rt)
                      (find-rt-max-x (xnode-left rt))
                      (find-rt-max-x (xnode-right rt)))]
    [else (max (find-rt-max-x (ynode-below rt))
               (find-rt-max-x (ynode-above rt)))]))

;; (rt-max-x rt)
;; produce the maximum value of any XNode in rt
;; or produce 'None if there are no XNodes in rt
;; rt-max-x: RTree -> (anyof Nat 'None) 
;; examples:
(check-expect (rt-max-x 'a) 'None)
(check-expect (rt-max-x (make-ynode 8 'a 'b)) 'None)
(check-expect (rt-max-x (make-xnode 8 'a 'b)) 8)

(define (rt-max-x rt)
  (cond
    [(not (x-node-in-rt? rt)) 'None]
    [else (find-rt-max-x rt)]))


;; tests:
(check-expect (rt-max-x 'a) 'None)
(check-expect (rt-max-x (make-xnode 0 'a 'b)) 0)
(check-expect (rt-max-x (make-xnode 1234 (make-xnode 12 'a 'b) 'c)) 1234)
(check-expect (rt-max-x (make-xnode 1234 (make-xnode 12345 'a 'b) 'c)) 12345)
(check-expect (rt-max-x (make-ynode 12 's 'd)) 'None)
(check-expect (rt-max-x
               (make-ynode 1
                           (make-xnode 1234 (make-xnode 12345 'a 'b) 'c)
                           (make-xnode 123456 'a 'b))) 123456)

;;
;; Q3d
;;

(define-struct win (wid rtree))
;; A Win (Window) is a (make-win Nat RTree)

;; (define (move-rtree rtree x y) moves the RTree rtree x pixels along the
;;   x-axis and y pixels along the y-axis.
;; move-rtree: RTree Int Int -> RTree
;; examples:
(check-expect (move-rtree (make-xnode 5 'a 'b) 4 2) (make-xnode 9 'a 'b))
(check-expect (move-rtree (make-xnode 5 (make-ynode 4 'a 'b) 'b) 2 3)
              (make-xnode 7 (make-ynode 7 'a 'b) 'b))

(define (move-rtree rtree x y)
  (cond
    [(symbol? rtree) rtree]
    [(xnode? rtree)
     (make-xnode (+ (xnode-val rtree) x)
                 (move-rtree (xnode-left rtree) x y)
                 (move-rtree (xnode-right rtree) x y))]
    [else
     (make-ynode (+ (ynode-val rtree) y)
                 (move-rtree (ynode-below rtree) x y)
                 (move-rtree (ynode-above rtree) x y))]))

;; (move-win wi x y)
;; moves the Win wi, x pixels along the x-axis
;; and y pixels in the y-axis.
;; move-win: Win Int Int -> Win
;; examples:
(check-expect (move-win (make-win 1 t1) -5 -5)
              (make-win 1 (move-rtree t1 -5 -5)))
(check-expect (move-win (make-win 2 t2) 5 -5)
              (make-win 2 (move-rtree t2 5 -5)))

(define (move-win wi x y)
  (make-win (win-wid wi)
            (move-rtree (win-rtree wi) x y)))

;; tests:
(check-expect (move-win (make-win 0
                                  (make-xnode 45
                                              (make-xnode 40 'a 'b)
                                              (make-ynode 68 'a
                                                          (make-ynode 30 'b 'c)
                                                          ))) 6 5)
(make-win 0 (make-xnode 51
                        (make-xnode 46 'a 'b)
                        (make-ynode 73 'a
                                    (make-ynode 35 'b 'c)))))
(check-expect (move-win
               (make-win 0 (make-ynode 30 'a 'b)) 1 10)
              (make-win 0 (make-ynode 40 'a 'b)))

(check-expect (move-win (make-win 0
                                  (make-xnode 45
                                              (make-xnode 40 'a 'b)
                                              (make-ynode 68 'a
                                                          (make-ynode 30 'b 'c)
                                                          ))) -6 -5)
              (make-win 0 (make-xnode 39
                        (make-xnode 34 'a 'b)
                        (make-ynode 63 'a
                                    (make-ynode 25 'b 'c)))))

(check-expect (move-win (make-win 0
                                  (make-xnode 45
                                              (make-xnode 40 'a 'b)
                                              (make-ynode 68 'a
                                                          (make-ynode 30 'b 'c)
                                                          ))) -41 -32)
              (make-win 0 (make-xnode 4
                        (make-xnode -1 'a 'b)
                        (make-ynode 36 'a
                                    (make-ynode -2 'b 'c)))))

(check-expect (move-win (make-win 0 t1) 5 5)
              (make-win 0 (move-rtree t1 5 5)))
(check-expect (move-win (make-win 1 t1) -5 -5)
              (make-win 1 (move-rtree t1 -5 -5)))
(check-expect (move-win (make-win 2 t2) 5 -5)
              (make-win 2 (move-rtree t2 5 -5)))

;;
;; Q3e
;;


;; A WinSys is one of:
;; * empty
;; * (cons Win WinSys)

;; (winsys-lookup ws pos)
;; produces the wid of the top window in ws that contains pos
;; and the Sym correspond to the rectange that contains pos
;; winsys-lookup: WinSys Posn -> (list Nat Sym)
;; examples:
(check-expect (winsys-lookup (list (make-win 1 t1)
                                   (make-win 2 t2))
                             (make-posn 0 0))
              (list 1 'Content))
(check-expect (winsys-lookup (list (make-win 1 t1)
                                   (make-win 2 t2))
                             (make-posn 100 100))
              (list 0 'None))
              
(define (winsys-lookup ws pos)
  (cond
    [(empty? ws) (list 0 'None)]
    [(or (symbol=? (rt-lookup (win-rtree (first ws)) pos) 'None)
         (symbol? (win-rtree (first ws))))
     (winsys-lookup (rest ws) pos)]
    [else (list (win-wid (first ws)) (rt-lookup (win-rtree (first ws)) pos))]))

;; tests:
(check-expect (winsys-lookup (list (make-win 1 t1)
                                   (make-win 2 t2))
                             (make-posn 0 0))
              (list 1 'Content))
(check-expect (winsys-lookup (list (make-win 1 t1)
                                   (make-win 2 t2))
                             (make-posn 100 100))
              (list 0 'None))
(check-expect (winsys-lookup empty (make-posn -10 -10)) (list 0 'None))
(check-expect (winsys-lookup (list (move-win (make-win 1 t1) 50 0)
                                   (make-win 2 t2)) (make-posn 20 0))
              (list 2 'Content))
(check-expect (winsys-lookup (list (make-win 1 t1)
                                   (make-win 2 t2))
                             (make-posn 60 60))
              (list 1 'Content))
(check-expect (winsys-lookup (list (move-win (make-win 1 t1) -95 -95)
                                   (make-win 2 t2))
                             (make-posn 60 60))
              (list 2 'x))
