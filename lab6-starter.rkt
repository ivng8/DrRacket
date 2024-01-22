;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab6-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the definitions below...

; A Position is a (make-posn Real Real)
; Interpretation: an (x, y) coordinate

; matching-x-position : [List-of Position] Real Position -> Position
; Finds the first position in the list with the given x-coordinate, or
; produces the given position if no such position can be found.

(check-expect
 (matching-x-position
  (list)
  10 (make-posn 0 0))
 (make-posn 0 0))

(check-expect
 (matching-x-position
  (list (make-posn 1 2)
        (make-posn 3 4))
  3 (make-posn 5 6))
 (make-posn 3 4))

(define (matching-x-position-old lop desired-x default)
  (cond [(empty? lop) default]
        [(cons? lop)
         (if (= (posn-x (first lop)) desired-x)
             (first lop)
             (matching-x-position (rest lop) desired-x default))]))


; string-with-length : [List-of String] Nat -> String
; Find the first string in the list with the given length, or produce
; "no such string" if no such String can be found.

(check-expect
 (string-with-length
  (list)
  10)
 "no such string")

(check-expect
 (string-with-length
  (list "hi" "hello" "aloha")
  5)
 "hello")

(define (string-with-length-old los desired-length)
  (cond [(empty? los) "no such string"]
        [(cons? los)
         (if (= (string-length (first los)) desired-length)
             (first los)
             (string-with-length (rest los) desired-length))]))


; TODO 1/3: Design the function find-first-match that abstracts the
;           two functions defined above. Don't forget to provide a
;           signature that is as general as possible, while still
;           not over-promising :)
;
;           Hint: one way to approach this is to think that a
;           function is being applied to every element in the
;           supplied list, which converts it from its original type
;           into a number - now just look for the desired number!

(check-expect (find-first-match
               (list) 10 (make-posn 0 0)
               posn-x)
              (make-posn 0 0))

(check-expect (find-first-match
               (list (make-posn 1 2)
                     (make-posn 3 4))
               3 (make-posn 5 6)
               posn-x)
              (make-posn 3 4))

(check-expect (find-first-match
               (list)
               10 "no such string"
               string-length)
              "no such string")

(check-expect (find-first-match
               (list "hi" "hello" "aloha")
               5 "no such string"
               string-length)
              "hello")

; find-first-match : (X Y) [List-of X] Y X [X Y -> Boolean] -> X
; takes a list of a certain data type
; compares it to an input using an inputted boolean function
; and gives back a valid response in the same data type 
; outputs a default response of the data type
; if no valid entry is found

(define (find-first-match lox desired-y default-x f)
  (cond
    [(empty? lox) default-x]
    [(cons? lox)
     (if (equal? (f (first lox)) desired-y)
         (first lox)
         (find-first-match (rest lox) desired-y default-x f))]))

; TODO 2/3: Redefine matching-x-position and string-with-length using
;           find-first-match.

(define (matching-x-position lop desired-x default)
  (find-first-match lop desired-x default posn-x))

(define (string-with-length los desired-length)
  (find-first-match los desired-length "no such string" string-length))


; TODO 3/3: Design the function any-true? that returns #true if a
;           list of Boolean data contains at least one #true, otherwise
;           #false. Use find-first-match to do so. You have been
;           supplied some tests for clarity (which you can use in your
;           design, but should supplement).

(check-expect
 (any-true? (list #f #f #f))
 #false)

(check-expect
 (any-true? (list #f #t #t))
 #true)

; any-true? : [List-of Boolean] -> Boolean
; takes a list of Boolean and determines if it has a #t

(define (any-true? lob)
  (find-first-match lob #f #f false?))
