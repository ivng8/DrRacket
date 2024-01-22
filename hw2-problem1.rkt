;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A Guess is a character of the following
; - "A"
; - "c"
; - "E"
; - "f"
; - "7"
; - "?"
; - "Z"
; Interpretation: represents the character guessed
; to get a valid response to a 5-option multiple-choice question


; Template:
; valid5mc?: Guess -> Boolean
(define (valid5mc-temp guess)
  (...
   (cond
     [(string=? "A") ...]
     [(string=? "c") ...])))

; valid-5c?: 1String -> Boolean
; Tells you if your input is a valid answer to a multiple-choice
(check-expect (valid5mc? "A") #true)
(check-expect (valid5mc? "c") #true)
(check-expect (valid5mc? "E") #true)
(check-expect (valid5mc? "f") #false)
(check-expect (valid5mc? "7") #false)
(check-expect (valid5mc? "?") #false)
(check-expect (valid5mc? "Z") #false)

(define (valid5mc? guess)
  (cond
    [(string=? (string-upcase guess) (string-upcase "A")) #true]
    [(string=? (string-upcase guess) (string-upcase "c")) #true]
    [(string=? (string-upcase guess) (string-upcase "E")) #true]
    [(string=? (string-upcase guess) (string-upcase "f")) #false]
    [(string=? (string-upcase guess) (string-upcase "7")) #false]
    [(string=? (string-upcase guess) (string-upcase "?")) #false]
    [(string=? (string-upcase guess) (string-upcase "Z")) #false]))

; TODO 1/1: Design the function valid-5mc? that determines if a supplied
;           1-character string (a 1String) is a valid response to a 5-option
;           multiple-choice question, either upper- or lower-case. So "A",
;           "c" and "E" are all valid (and so the function should return #true;
;           whereas "f", "7", "?", and "Z" are all invalid (and so the function
;           should return #false).
;
;           Be sure you follow all steps of the design recipe and include
;           check-expects that cover both result values, as well as making sure
;           upper- and lower-case examples work properly.
;
;           Importantly, you should NOT use code that follows the pattern...
;
;              (if expression #true #false)
;
;           since this could be replaced with just expression



