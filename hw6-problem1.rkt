;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In this problem you'll practice with lists by designing a function to
; help bookstores!

; Most bookstores sort the books on their shelves by the author’s last
; name. Unfortunately, some bookstore patrons do not preserve this order
; when browsing books, and simply place books back wherever they fit.

; Assume that bookstores keep all authors whose last name starts with the
; same letter on the same shelf, and those shelves are labeled with that
; letter. A record of which authors are on a given shelf would be represented
; using the following data definitions:

(define-struct shelf [letter authors])

; A Shelf is a (make-shelf 1String [List-of String])
; Interpretation: a record of the letter the authors' last name *should* start
; with, and the list of the *actual* last names on the shelf.

(define SHELF-1 (make-shelf "A" (list "Austen" "Hurston" "Angelou" "Butler" "Alvarez")))
(define SHELF-2 (make-shelf "B" (list)))
(define SHELF-3 (make-shelf "C" (list "Carle" "Coates")))
(define SHELF-4 (make-shelf "D" (list "Donna" "Michael")))

(define (shelf-temp s)
  (... (shelf-letter s) ...
       (list-of-string-temp (shelf-authors s)) ...))


; TODO 1/1: Design the function fix-shelves that takes a list of Shelf records
;           and produces a list of Shelf records where at least one author does
;           not belong on the Shelf. The output Shelf records should only contain
;           the authors who don't belong on that shelf. Shelf records and the authors
;           within those records should be in the same order in the output as they
;           appear in the input. Do not generate empty Shelf records; this generates
;           needlessly long reports, which annoys the employees. You have been
;           supplied a test for clarity (which you can use in your design, but
;           should supplement). Make sure your solution follows the (list) templates!

; A ListofShelves (LoSh) is on of:
; - '()
; - (cons Shelf LoSh)
; Interpretation: a list of shelves

(define (losh-temp losh)
  (...
   (cond
     [(empty? losh) ...]
     [(cons? losh) ...
      (shelf-temp (first losh)) ...
      (losh-temp (rest losh)) ...])))

(check-expect (fix-shelves (list SHELF-1 SHELF-2 SHELF-3))
              (list (make-shelf "A" (list "Hurston" "Butler"))))
(check-expect (fix-shelves (list SHELF-1 SHELF-2 SHELF-3 SHELF-4))
              (list (make-shelf "A" (list "Hurston" "Butler"))
                    (make-shelf "D" (list "Michael"))))

; fix-shelves : [List-of Shelf] -> [List-of Shelf]
; makes a list of filtered out authors that do not belong on the shelf
; that they are on

(define (fix-shelves losh)
  (cond
    [(empty? losh) '()]
    [(cons? losh)
     (if (empty? (shelf-authors (single-shelf (first losh))))
         (fix-shelves (rest losh))
         (cons
          (single-shelf (first losh))
          (fix-shelves (rest losh))))]))


(check-expect (single-shelf SHELF-1) (make-shelf "A" (list "Hurston" "Butler")))
(check-expect (single-shelf SHELF-2) (make-shelf "B" (list)))
(check-expect (single-shelf SHELF-3) (make-shelf "C" (list)))

; single-shelf : Shelf -> Shelf
; makes a new shelf with the original 1String
; and a list of the authors that do not belong there

(define (single-shelf s)
  (make-shelf (shelf-letter s)
              (list-of-authors (shelf-letter s) (shelf-authors s))))


(check-expect (list-of-authors "A" (list "Austen" "Hurston" "Angelou" "Butler" "Alvarez"))
              (list "Hurston" "Butler"))
(check-expect (list-of-authors "B" (list)) (list))
(check-expect (list-of-authors "C" (list "Carle" "Coates")) (list))

; list-of-authors : 1String [List-of Authors] -> [List-of Authors]
; creates a list of authors that do not belong there

(define (list-of-authors s loa)
  (cond
    [(empty? loa) (list)]
    [(cons? loa)
     (if (string=? s (substring (first loa) 0 1))
         (list-of-authors s (rest loa))
         (cons (first loa) (list-of-authors s (rest loa))))]))