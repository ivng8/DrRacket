;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let's return to Wordle (https://www.nytimes.com/games/wordle/)!

; Eventually we'll want to visualize a prior guess, which means categorizing
; each letter as either correct (i.e., used letter in the correct spot),
; misplaced (i.e., used letter, but in the wrong spot), or wrong (i.e.,
; the letter is not in the word in any spot).

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

; TODO 1/2: Design the data type LetterStatus (LS), which represents the three
;           categories of letters described above. Make sure to follow all steps
;           of the design recipe for data!

; ls->color: ls -> String
; Takes the status of the letter and gives the corresponding color

(check-expect (ls->color correct) "green")
(check-expect (ls->color misplaced) "yellow")
(check-expect (ls->color wrong) "gray")

(define (ls->color ls)
  (cond
    [(string=? ls correct) "green"]
    [(string=? ls misplaced) "yellow"]
    [(string=? ls wrong) "gray"]))

; TODO 2/2: Design the function ls->color, which accepts a LetterStatus and
;           produces an associated Color (an existing type - look up the
;           image-color? function for details and examples). In the NYT game,
;           correct letters are green, misplaced letters are yellow, and wrong
;           letters are gray - feel free to choose the shades of these colors
;           that bring you happiness :) And make sure to follow all steps of
;           the design recipe for functions!



