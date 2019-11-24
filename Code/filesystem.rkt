;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname filesystem) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;;    Minhall Shen
;;    CS135 Fall 2019
;;    Assignment 8 Question 3
;; ***************************************************
;;
;------------------------------------------------------------------------------

(require "filesystem-lib.rkt")

;; A Filesystem Object (FSObject) is one of:
;; * File
;; * Dir

(define-struct file (name size owner))
;; A File is a (make-file Str Nat Sym)

(define-struct dir (name owner contents))
;; A Dir is a (make-dir Str Sym (listof FSObject))

;; constants:
(define example-fs-no-empty (make-dir
                             "root"
                             'root
                             (list
                              (make-dir "Dan" 'dan
                                        (list (make-file "log.txt" 768 'dan)
                                              (make-file "profile.jpg" 60370 'dan)
                                              (make-dir "music" 'dan
                                                        (list (make-file "Thelonius Monk.mp3" 92227584 'dan)))))
                              (make-dir "Slides" 'teaching (list (make-dir "cs135" 'teaching
                                                                           (list (make-file "01-intro.pdf" 72244 'teaching)
                                                                                 (make-file "11-trees.pdf" 123124 'teaching)
                                                                                 (make-dir "system" 'root empty)))))
                              (make-file "vmlinuz" 30 'root))))

(define example-fs
  (make-dir "root" 'root
            (list
             (make-dir "Dan" 'dan
                       (list (make-file "log.txt" 768 'dan)
                             (make-file "profile.jpg" 60370 'dan)
                             (make-dir "music" 'dan
                                       (list (make-file "Thelonius Monk.mp3" 92227584 'dan)))))
             (make-dir "Slides" 'teaching
                       (list (make-dir "cs135" 'teaching
                                       (list (make-file "01-intro.pdf" 72244 'teaching)
                                             (make-file "11-trees.pdf" 123124 'teaching)
                                             (make-dir "system" 'root
                                                       (list (make-dir "logs" 'teaching empty)))))))
             (make-file "vmlinuz" 30 'root))))

;; A
(define (fsobject-template fso)
  (cond
    [(file? fso)
     (... (file-name fso) ...
          (file-size fso) ...
          (file-owner fso) ...)]
    [(dir? fso)
     (... (dir-name fso) ...
          (dir-owner fso) ...
          (listof-fsobject-template (dir-contents fso) ...))]))

(define (listof-fsobject lofso)
  (cond
    [(empty? lofso) ...]
    [else (... (fsobject-template (first lofso))
               ... (listof-fsobject (rest lofso)) ...)]))

;; B

;; (fsobject-name fso) consumes an FSObject and produces its name/
;; fsobject: FSObject -> Str
;; examples:
(check-expect (fsobject-name (make-file "hello.txt" 32 'Dan))
              "hello.txt")
(check-expect (fsobject-name (make-dir "My Music" 'nobody empty))
              "My Music")

(define (fsobject-name fso)
  (cond
    [(file? fso)
     (file-name fso)]
    [(dir? fso)
     (dir-name fso)]))

;; tests:
(check-expect (fsobject-name (make-file "hello.txt" 32 'Dan))
              "hello.txt")
(check-expect (fsobject-name (make-dir "My Music" 'nobody empty))
              "My Music")
(check-expect (fsobject-name
               (make-dir "Code" 'Minhall
                         (list (make-file "unzip.rkt" 20 'Minhall)
                               (make-file "filesystem.rkt" 50 'Minhall)
                               (make-file "html.rkt" 40 'Minhall))))
              "Code")
;; C

;; (count-files fso) consumes a FSObject and produces the number Files in the
;;   FSObject.
;; count-files: FSObject -> Nat
;; examples:
(check-expect (count-files (make-file "README" 16 'me)) 1)
(check-expect (count-files example-fs) 6)

(define (count-files fso)
  (+ (cond
       [(file? fso) 1]
       [else (local
               [;; (count-files-in-dir dir) consumes a Dir and counts the
                ;;    number of Files in the Dir.
                ;; count-files-in-dir: Dir -> Nat
                (define (count-files-in-dir dir)
                  (cond
                    [(empty? (dir-contents dir)) 0]
                    [else (+ (count-files (first (dir-contents dir)))
                             (count-files-in-dir
                              (make-dir (dir-name dir)
                                        (dir-owner dir)
                                        (rest (dir-contents dir)))))]))]
               (count-files-in-dir fso))])))

;; tests:
(check-expect (count-files (make-file "README" 16 'me)) 1)
(check-expect (count-files example-fs) 6)
(check-expect (count-files  (make-dir "a" 1234
                                      (list (make-file "b" 1 'ok)
                                            (make-file "c" 2 'asd))))
              2)

;; D

;; (file-exists? fso los) consumes a FSObject and a list of Strings and returns
;;   true if the list of strings represents a path to a File and false
;;   otherwise.
;; file-exists?: FSObject (listof String) -> Bool
;; examples:
(check-expect
 (file-exists? example-fs (list "root" "Slides" "cs135" "11-trees.pdf")) true)
(check-expect (file-exists? example-fs (list "root" "Dan")) false)
(check-expect (file-exists? example-fs (list "readme.txt")) false)

(define (file-exists? fso los)
  (cond
    [(empty? los) false]
    [(and (file? fso)
          (string=? (file-name fso)
                    (first los))
          (empty? (rest los))) true]
    [(and (file? fso)
          (not (string=? (file-name fso)
                         (first los)))
          (empty? (rest los))) false]
    [else
     (local
       [;; (in-fso? lofso los) consumes a list of FSObjects and a list of Str
        ;;   and produces true if the list of Str is a valid path to a file.
        ;; in-fso?: (listof FSObject) (listof Str) -> Bool        
        (define (in-fso? lofso los)
          (cond
            [(empty? lofso) false]
            [(file-exists? (first lofso) los) true]
            [else (in-fso? (rest lofso) los)]))]
       (cond
         [(and (not (empty? (rest los)))
               (string=? (dir-name fso)
                         (first los)))
          (in-fso? (dir-contents fso)
                   (rest los))]
         [else false]))]))
            
;; tests:
(check-expect
 (file-exists? example-fs (list "root" "Slides" "cs135" "11-trees.pdf")) true)
(check-expect (file-exists? example-fs (list "root" "Dan")) false)
(check-expect (file-exists? example-fs (list "readme.txt")) false)
(check-expect (file-exists? example-fs empty) false)
(check-expect (file-exists? example-fs (list "root" "vmlinuz")) true)                    

;; E

;; (remove-empty fso) consumes a FSObject and produces a new FSObject with all
;;   empty directories removed.
;; remove-empty: FSObject -> FSObject
;; example:
(check-expect (remove-empty example-fs) example-fs-no-empty)

(define (remove-empty fso)
  (cond
    [(file? fso) fso]
    [else
     (local
       [;; (remove-empty-dir lofso) consumes a list of FSObjects and produces
        ;;   a FSObject with all empty directories removed.
        ;; remove-empty-dir: (listof FSObject) -> FSObject
        (define (remove-empty-dir lofso)
          (cond
            [(empty? lofso) empty]
            [(file? (first lofso))
             (cons (first lofso) (remove-empty-dir (rest lofso)))]
            [(cons? (dir-contents (first lofso)))
             (cons (make-dir
                    (dir-name (first lofso))
                    (dir-owner (first lofso))
                    (remove-empty-dir (dir-contents (first lofso))))
                   (remove-empty-dir (rest lofso)))]
            [(empty? (dir-contents (first lofso)))
             (rest lofso)]))]
       (make-dir (dir-name fso)
                 (dir-owner fso)
                 (remove-empty-dir (dir-contents fso))))]))

;; tests:
(check-expect (remove-empty example-fs) example-fs-no-empty)
(check-expect (remove-empty (make-file "a" 12 'me)) (make-file "a" 12 'me))
    
;; F

;; (disk-hog fso) consumes a FSObject and produces the symbol that owns the
;;   largest File, and false if there are no files.
;; disk-hog: FSObject -> Sym
;; examples:
(check-expect (disk-hog example-fs) 'dan)
(check-expect (disk-hog (make-file "a" 12 'me)) 'me)

(define (disk-hog fso)
  (local
    [;; (count-files fso) consumes a FSObject and produces the number Files in 
     ;;   FSObject.
     ;; count-files: FSObject -> Nat
     (define (count-files fso)
       (+ (cond
            [(file? fso) 1]
            [else (local
                    [;; (count-files-in-dir dir) consumes a Dir and counts the
                     ;;    number of Files in the Dir.
                     ;; count-files-in-dir: Dir -> Nat
                     (define (count-files-in-dir dir)
                       (cond
                         [(empty? (dir-contents dir)) 0]
                         [else (+ (count-files (first (dir-contents dir)))
                                  (count-files-in-dir
                                   (make-dir (dir-name dir)
                                             (dir-owner dir)
                                             (rest (dir-contents dir)))))]))]
                    (count-files-in-dir fso))])))
     ;; (lofile fso) consumes a FSObject and produces a list of Files in the
     ;;   fso.
     ;; lofile: FSObject -> (listof File)
     (define (lofile fso)
       (cond
         [(file? fso) (cons fso empty)]
         [else
          (local
            [;; (lofile-in-dir dir) consumes a Dir and produces a list of Files
             ;;   in the Dir.
             ;; lofile-in-dir: Dir -> (listof File)
             (define (lofile-in-dir dir)
               (cond
                 [(empty? (dir-contents dir)) empty]
                 [else (append (lofile (first (dir-contents dir)))
                               (lofile-in-dir
                                (make-dir (dir-name dir)
                                          (dir-owner dir)
                                          (rest (dir-contents dir)))))]))]
            (lofile-in-dir fso))]))
     ;; (max-lofile lofile) consumes a list of File and produces the File with
     ;;    the largest size.
     ;; max-lofile: (listof File) -> File
     (define (max-lofile lofile)
       (cond
         [(empty? (rest lofile)) (first lofile)]
         [(> (file-size (first lofile))
             (file-size (max-lofile (rest lofile))))
          (first lofile)]
         [else (max-lofile (rest lofile))]))]
    (cond [(= (count-files fso) 0) false]
          [else (file-owner (max-lofile (lofile fso)))])))

;; tests:
(check-expect (disk-hog example-fs) 'dan)
(check-expect (disk-hog (make-file "a" 12 'me)) 'me)
(check-expect (disk-hog example-fs-no-empty) 'dan)
(check-expect (disk-hog (make-dir "a" 'a
                                  (list (make-file "b" 12 's)
                                        (make-file "a" 12 'd)))) 'd)
(check-expect (disk-hog (make-dir "secrets" 'cia empty)) false)

;; G

;; (owned-by fso sym) consumes a FSObject and a Symbol and produces a list of
;;   paths to all FSObject owned by that user.
;; owned-by: FSObject Sym -> (listof (listof Str))
;; examples:

;; H (bonus)

