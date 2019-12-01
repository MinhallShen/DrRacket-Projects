;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname travel-map) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 10 Question 1
;; ***************************************************
;;
;------------------------------------------------------------------------------

;; constants:
(define southern-ontario
  '((Brantford ((Hamilton 30)))
    (Cambridge ((Brantford 30) (Hamilton 45) (London 70) (TO 80)))
    (Guelph ((Cambridge 30) (TO 80)))
    (Hamilton ())
    (London ((Brantford 70)))
    (KW ((Cambridge 30) (Guelph 35) (Stratford 40)))
    (Stratford ((London 50)))
    (TO ((Hamilton 60)))))

;; A Node is a Sym
;; A Map is a (listof (list Node (listof Neighbour)))
;; requires: Map is directed and acyclic
;; A Neighbour is a (list Node Nat) where the number indicates the
;; travel time (in minutes) to the neighbour.

;; a)
;; (travel-time orig dest graph) consumes an origin, a destination and a grapha
;;   and produces the travel time between the origin and destination. If there
;;   is no path from the origin to the destination, it produces false.
;; travel-time: Sym Sym graph -> (anyof Num false)
;; Requires: orig and dest must be on the map, graph is acyclic.
;; examples:
(check-expect (travel-time 'Guelph 'Hamilton southern-ontario) 90)
(check-expect (travel-time 'Brantford 'Hamilton southern-ontario) 30)

(define (travel-time orig dest G)
  (local
    [(define (find-route orig dest G)
       (cond [(symbol=? orig dest) (list dest)]
             [else (local [(define (get-cities lonbr)
                             (cond [(empty? lonbr) empty]
                                   [else (cons (first (first lonbr))
                                               (get-cities (rest lonbr)))]))
                           (define (neighbours v G)
                             (cond [(symbol=? v (first (first G)))
                                    (get-cities (second (first G)))]
                                   [else (neighbours v (rest G))]))
                           (define nbrs (neighbours orig G))
                           (define route (find-route/list nbrs dest G))]
                     (cond [(false? route) false]
                           [else (cons orig route)]))]))
     (define (find-route/list lov dest G)
       (cond [(empty? lov) false]
             [else (local [(define result (find-route (first lov) dest G))]
                     (cond [(false? result) (find-route/list (rest lov) dest G)]
                           [else result]))]))
     (define (get-time lonbr sym)
       (cond
         [(symbol=? sym (first (first lonbr)))
          (second (first lonbr))]
         [else (get-time (rest lonbr) sym)]))
     (define (get-time/map orig dest G)
       (cond
         [(symbol=? orig (first (first G)))
          (get-time (second (first G)) dest)]
         [else (get-time/map orig dest (rest G))]))
     (define the-route (find-route orig dest G))
     (define (travel-route-time route G)
       (cond
         [(false? route) false]
         [(empty? (rest route)) 0]
         [else (+ (get-time/map (first route)
                                (second route) G)
                  (travel-route-time (rest route) G))]))]
    (travel-route-time the-route G)))

;; tests:
(check-expect (travel-time 'Guelph 'Hamilton southern-ontario) 90)
(check-expect (travel-time 'Brantford 'Hamilton southern-ontario) 30)
(check-expect (travel-time 'Hamilton 'Hamilton southern-ontario) 0)
(check-expect (travel-time 'Hamilton 'TO southern-ontario) false)
(check-expect (travel-time 'KW 'Hamilton southern-ontario) 90)
(check-expect (travel-time 'Stratford 'Hamilton southern-ontario) 150)
(check-expect (travel-time 'KW 'Guelph southern-ontario) 35)

;; b)
;; (all-paths orig dest G) consumes an origin, a destination and a map and
;;   produces a list of all valid paths.
;; all-paths: Sym Sym Map -> (listof (listof Node))
;; Requires: orig and dest are in the map.
;; examples:
(check-expect (all-paths 'Stratford 'Guelph southern-ontario) empty)
(check-expect
 (all-paths 'Guelph 'Hamilton southern-ontario)
 '((Guelph Cambridge Brantford Hamilton)
   (Guelph Cambridge Hamilton)
   (Guelph Cambridge London Brantford Hamilton)
   (Guelph Cambridge TO Hamilton)
   (Guelph TO Hamilton)))

(define (all-paths orig dest G)
  (local
    [(define (find-routes city path-so-far)
       (cond
         [(symbol=? dest city) (list (append path-so-far (list city)))]
         [(empty? (neighbours city G)) empty]  
         [else (find-routes/list (append path-so-far (list city))
                                 (neighbours city G))]))
     (define (find-routes/list path-so-far lonbr)
       (cond
         [(empty? lonbr) empty]
         [else (append (find-routes (first lonbr) path-so-far)
                       (find-routes/list path-so-far (rest lonbr)))]))
     (define (get-cities neighbourhood)
       (cond
         [(empty? neighbourhood) empty]
         [else (cons (first (first neighbourhood))
                     (get-cities (rest neighbourhood)))]))
     (define (neighbours v G)
       (cond [(symbol=? v (first (first G)))
              (get-cities (second (first G)))]
             [else (neighbours v (rest G))]))]
    (find-routes orig empty)))

;; tests:
(check-expect (all-paths 'Stratford 'Guelph southern-ontario) empty)
(check-expect
 (all-paths 'Guelph 'Hamilton southern-ontario)
 '((Guelph Cambridge Brantford Hamilton)
   (Guelph Cambridge Hamilton)
   (Guelph Cambridge London Brantford Hamilton)
   (Guelph Cambridge TO Hamilton)
   (Guelph TO Hamilton)))
(check-expect (all-paths 'Brantford 'Hamilton southern-ontario)
              '((Brantford Hamilton)))
(check-expect (all-paths 'KW 'Guelph southern-ontario)
              '((KW Guelph)))
              

;; c)
;; (all-travel-times orig dest G) consumes an origin and a destination and 
;;    produces a list of all the possible paths as well as their travel time.
;; all-travel-times: Sym Sym Map -> (listof (list Nat (listof Sym)))
;; Requires: orig and dest must be in the map.
;; examples:
(check-expect (all-travel-times 'Stratford 'Guelph southern-ontario) empty)
(check-expect
 (all-travel-times 'Guelph 'Hamilton southern-ontario)
 '((90 (Guelph Cambridge Brantford Hamilton))
   (75 (Guelph Cambridge Hamilton))
   (200 (Guelph Cambridge London Brantford Hamilton))
   (170 (Guelph Cambridge TO Hamilton))
   (140 (Guelph TO Hamilton))))

(define (all-travel-times orig dest G)
  (local
    [(define (find-time lonbr sym)
       (cond [(symbol=? (first (first lonbr)) sym)
              (second (first lonbr))]
             [else (find-time (rest lonbr) sym)]))

     (define (t orig dest G)
       (cond [(symbol=? orig (first (first G)))
              (find-time (second (first G)) dest)]
             [else (t orig dest (rest G))]))

     (define (travel-time route G)
       (cond 
         [(empty? (rest route)) 0]
         [else (+ (t (first route)
                     (second route) G)
                  (travel-time (rest route) G))]))
     (define (time-list loroute)
       (cond
         [(empty? loroute) empty]
         [else (cons (travel-time (first loroute) G)
                     (time-list (rest loroute)))]))]
    (map (lambda (t route) (list t route))
         (time-list (all-paths orig dest G))
         (all-paths orig dest G))))

;; tests:
(check-expect (all-travel-times 'Stratford 'Guelph southern-ontario) empty)
(check-expect
 (all-travel-times 'Guelph 'Hamilton southern-ontario)
 '((90 (Guelph Cambridge Brantford Hamilton))
   (75 (Guelph Cambridge Hamilton))
   (200 (Guelph Cambridge London Brantford Hamilton))
   (170 (Guelph Cambridge TO Hamilton))
   (140 (Guelph TO Hamilton))))
(check-expect (all-travel-times 'Brantford 'Hamilton southern-ontario)
              '((30 (Brantford Hamilton))))
(check-expect (all-travel-times 'KW 'Guelph southern-ontario)
              '((35 (KW Guelph))))
    

    

     
           
         
         
    


          

    

        
        
        
    
     




        
