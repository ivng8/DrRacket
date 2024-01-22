;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; IMPORTANT:
; 1. The functions that you design for this problem *must* use the ISL list
;    abstraction(s); you MAY NOT use recursion: doing so will lead you to get
;    no code credit for the function :(
;
; 2. The planning part of this problem is in the SAME file used for ALL parts
;    of Homework 8.


; Your task here will be to design the function elim-contains-char, which takes
; a list of strings and produces a list of the same strings, in the same order,
; but excluding those strings that contain a supplied character (represented as
; a 1String). For clarity, here is the intended signature:

(define ABC (list "apple" "Bee" "Carrot"))
(define AAA (list "apple" "Acorn" "Aegyo"))
(define BBC (list "Bacon" "bagel" "carrot"))

; elim-contains-char : 1String [List-of String] -> [List-of String]
; takes a list of strings and produces a list
; of the same strings, in the same order,
; but excluding strings that contain a supplied character

(check-expect (elim-contains-char "a" (list)) (list))
(check-expect (elim-contains-char "a" ABC) (list "Bee"))
(check-expect (elim-contains-char "B" ABC) (list "apple" "Carrot"))
(check-expect (elim-contains-char "c" AAA) (list "apple" "Aegyo"))
(check-expect (elim-contains-char "a" BBC) (list))
(check-expect (elim-contains-char "c" BBC) (list "bagel"))

(define (elim-contains-char s los)
  (local [; letter-compare : String -> Boolean
          ; compares a String to a 1String and returns true
          ; if the inputted 1String does not match any letter in the String
          (define (letter-compare str)
            (andmap (λ (x) (not (string-ci=? s x))) (explode str)))]
    (filter
     letter-compare
     los)))

; Note: For purposes of this problem, you should NOT use string-contains? (or
;       similar). Instead, use the explode function to treat the supplied
;       string as a list of characters (each represented as a 1String).


; TODO 1/2: Plan your solution, using the planning interface described on the
;           canvas page for this assignment. ALL PLANNING FOR THIS HW WILL BE
;           DONE IN THE SAME PLACE, AND SUBMITTED TOGETHER.

; TODO 2/2: Design the function elim-contains-char using the ISL list
;           abstractions. YOUR CODE SHOULD NOT USE ANY RECURSION.
