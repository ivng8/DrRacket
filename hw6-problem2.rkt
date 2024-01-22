;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; It's super useful to be able to answer the question: does a value appear
; in a list? But of course that question can be phrased multiple ways...

; TODO 1/3: Design two functions: string-in-list? and string-in-list-ci?.
;           The first returns #true if a supplied string appears
;           *exactly* in a list of strings, whereas the second returns
;           #true if a supplied string occurs in a list of strings if
;           we ignore lower/upper-case. You have been supplied some tests
;           for clarity (which you can use in your design, but should
;           supplement). Make sure your code follows the list template!

(check-expect (string-in-list? "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list? "A" (list "a" "b" "c")) #f)

; string-in-list? : String [List-of String] -> Boolean
; detects if supplised string appears exactly in list of supplied strings

(define (string-in-list? s los)
  (cond
    [(empty? los) #f]
    [(cons? los)
     (if (string=? s (first los)) #t
         (string-in-list? s (rest los)))]))

(check-expect (string-in-list-ci? "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list-ci? "A" (list "a" "b" "c")) #t)

; string-in-list-ci? : String [List-of String] -> Boolean
; detects if supplised string appears in list of supplied strings
; regardless of lower/upper case

(define (string-in-list-ci? s los)
  (cond
    [(empty? los) #f]
    [(cons? los)
     (if (string-ci=? s (first los)) #t
         (string-in-list-ci? s (rest los)))]))

; TODO 2/3: Those two functions probably feel rather similar - so now
;           design the abstraction value-in-list? based on these two
;           functions.

;           Notes:
;           - Think through your signature to make sure it is as general
;             as possible, while still not making promises your abstraction
;             cannot keep!
;           - Don't forget to improve your implementations for the last
;             step! (Importantly: keep the old code by renaming the
;             functions string-in-list?/old and string-in-list-ci?/old;
;             you do not need to change/reproduce any parts of the function
;             design recipe for these old function implementations.)

(check-expect (value-in-list? "a" (list "a" "b" "c") string=?) #t)
(check-expect (value-in-list? "A" (list "a" "b" "c") string=?) #f)
(check-expect (value-in-list? "a" (list "a" "b" "c") string-ci=?) #t)
(check-expect (value-in-list? "A" (list "a" "b" "c") string-ci=?) #t)

; value-in-list? : (X) X [List-of X] [X X -> Boolean] -> Boolean
; runs an inputted Boolean function that compares an input
; against a list of inputs that have the same data type

(define (value-in-list? x lox f)
  (cond
    [(empty? lox) #f]
    [(cons? lox)
     (if (f x (first lox)) #t
         (value-in-list? x (rest lox) f))]))

; TODO 3/3: Now put your fancy new abstraction to good use! Design the function
;           anything-bigger? that determines if any of a list of numbers is
;           bigger than a supplied number. You have been supplied some tests
;           for clarity (which you can use in your design, but should supplement).

(check-expect (anything-bigger? 5 (list 10 -1 3)) #t)
(check-expect (anything-bigger? 100 (list 10 -1 3)) #f)

; anything-bigger? : Real [List-of Real] -> Boolean
; runs an inputted Boolean function that compares an input
; against a list of inputs that have the same data type

(define (anything-bigger? x lox)
  (value-in-list? x lox <))