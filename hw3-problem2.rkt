;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Back to Wordle (https://www.nytimes.com/games/wordle/)!

; Recall that in HW2 you designed the type LetterStatus, an enumeration
; that categorized letters as correct, misplaced, or wrong.

; TODO 1/1: Design LetterStatusPair - data that represents pairing a letter
;           (1String) with its status (LetterStatus). Example values might
;           include that the the letter "A" is correct, "B" is wrong, or
;           "C" is misplaced.


; LetterStatus (LS) is one of the following
; - correct
; - misplaced
; - wrong
; Interpretation: describes the status of a letter in the guessing of a word
(define correct "correct")
(define misplaced "misplaced")
(define wrong "wrong")

; Template
; ls->color: String -> String
(define (ls->color-temp ls)
  (...
   (cond
     [(string=? ls correct) ...]
     [(string=? ls misplaced) ...]
     [(string=? ls wrong) ...])))


(define-struct lsp [str ls])

; LetterStatusPair is a combination of a 1String and LS
; - "A" "correct"
; - "B" "wrong"
; - "C misplaced"
; Interpretation: pairs a letter with the status of the letter

(define lsp-a (make-lsp "A" correct))
(define lsp-b (make-lsp "B" wrong))
(define lsp-c (make-lsp "C" misplaced))

(define (letterstatuspair-temp lsp)
  (... lsp ...))


;           Notes:
;           - You are welcome to use your own (correct) design of
;             LetterStatus, or ours (once released); either way, include
;             it below as a part of your solution to this problem.
;           - Follow all steps of the design recipe for data and remember
;             that in templates, if the type of a field is a data definition,
;             you need to call its associated template!




