;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Back to Wordle (https://www.nytimes.com/games/wordle/)!

; We are going to put together some work you did in prior assignments...
; - In HW1, you made code for a boxed-letter function, which could create a
;   letter in a box, given lots of specific details (about color, size, etc).
; - In HW2, you designed an ls->color function, which would return the color
;   associated with the status of a letter.
; - In HW3, you designed the LetterStatusPair data type, which could represent
;   the pairing of a letter with its status.

; Moving towards the full game, it's going to be handy to be able to more
; simply draw various boxed letters (e.g., those that are part of the current
; guess, as well as those that were previously guessed). So let's build & use
; some abstractions :)

; TODO 1/2: Re-design boxed-letter to work a bit differently: instead of being
;           supplied details of how to draw the box (e.g., background/border
;           colors), it takes a function to make one (given a size), as well as
;           a letter, and then places the letter onto the background produced
;           by the function (with a font size proportional to the box size).
;
;           For clarity, you have been provided a function to produce a "blank"
;           background (useful while typing a guess) - when combined with your
;           new boxed-letter function, the supplied test should pass. You should
;           supplement this test with others in your design.


(define BG-COLOR "white")
(define BORDER-COLOR "dimgray")
(define GUESS-COLOR "black")

; blank : NonNegReal -> Image
; produces a blank box in the appropriate size

(check-expect (blank 5)
              (overlay (square 5 "outline" BORDER-COLOR)
                       (square 5 "solid" GUESS-COLOR)))

(check-expect (blank 7)
              (overlay (square 7 "outline" BORDER-COLOR)
                       (square 7 "solid" GUESS-COLOR)))

(define (blank size)
  (overlay (square size "outline" BORDER-COLOR)
           (square size "solid" GUESS-COLOR)))

(check-expect
 (boxed-letter "B" 64 blank)
 (overlay
  (text "B" 32 BG-COLOR)
  (square 64 "outline" BORDER-COLOR)
  (square 64 "solid" GUESS-COLOR)))

; boxed-letter : (X) 1String X [X -> Image] -> Image
; takes inputted 1String and an image created by an
; inputted function and inputted variable

(define (boxed-letter s x f)
  (overlay
   (text s 32 BG-COLOR)
   (f x)))

; TODO 2/2: Now, use your boxed-letter abstraction to design two useful
;           functions: guess-letter->image and lsp->image. The former
;           produces a visualization of a letter on a blank background,
;           while the latter produces a visualization of a LetterStatusPair
;           (LSP) on a background associated with he letter's status.
;           You have been supplied some tests for clarity (which you can use
;           in your design, but should supplement).
;
;           You are welcome to use your own (correct) design of LetterStatus,
;           LetterStatusPair, or ours; either way, include it below as a part
;           of your solution to this problem. (Note that you might have to
;           adjust your constants and structure name to accommodate the
;           supplied tests.)


(define LT-SIZE 64)

; guess-letter->image : 1String -> Image
; produces a visualization of a letter on a blank background

(check-expect
 (guess-letter->image "A")
 (overlay
  (text "A" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "outline" BORDER-COLOR)
  (square LT-SIZE "solid" GUESS-COLOR)))

(check-expect
 (guess-letter->image "B")
 (overlay
  (text "B" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "outline" BORDER-COLOR)
  (square LT-SIZE "solid" GUESS-COLOR)))

(define (guess-letter->image s)
  (boxed-letter s LT-SIZE blank))

; LetterStatus (LS) is one of the following
; - correct
; - misplaced
; - wrong
; Interpretation: describes the status of a letter in the guessing of a word

(define CORRECT "correct")
(define MISPLACED "misplaced")
(define WRONG "wrong")

(define (ls-temp ls)
  (...
   (cond
     [(string=? ls CORRECT) ...]
     [(string=? ls MISPLACED) ...]
     [(string=? ls WRONG) ...])))

; ls->color : LS -> Color
; takes a LS and returns its corresponding color

(check-expect (ls->color CORRECT) "darkgreen")
(check-expect (ls->color WRONG) "dimgray")
(check-expect (ls->color MISPLACED) "goldenrod")

(define (ls->color ls)
  (cond
    [(string=? ls CORRECT) "darkgreen"]
    [(string=? ls MISPLACED) "goldenrod"]
    [(string=? ls WRONG) "dimgray"]))

(define-struct lsp [str ls])

; LetterStatusPair (LSP) is a (make-lsp 1String LS)
; Interpretation: pairs a letter with the status of the letter

(define lsp-a (make-lsp "A" CORRECT))
(define lsp-b (make-lsp "B" WRONG))
(define lsp-c (make-lsp "C" MISPLACED))

(define (letterstatuspair-temp lsp)
  (... (lsp-str lsp) ...
       (lsp-ls lsp) ...))

; filled : LS -> Image
; produces a box in the appropriate size and color

(check-expect (filled CORRECT) (square LT-SIZE "solid" "darkgreen"))
(check-expect (filled WRONG) (square LT-SIZE "solid" "dimgray"))
(check-expect (filled MISPLACED) (square LT-SIZE "solid" "goldenrod"))

(define (filled ls)
  (square LT-SIZE "solid" (ls->color ls)))

; lsp->image : LSP -> Image
; produces a visualization of a LSP
; on a background associated with he letter's status

(check-expect
 (lsp->image lsp-a)
 (overlay
  (text "A" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "solid" "darkgreen")))

(check-expect
 (lsp->image lsp-b)
 (overlay
  (text "B" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "solid" "dimgray")))

(check-expect
 (lsp->image lsp-c)
 (overlay
  (text "C" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "solid" "goldenrod")))

(define (lsp->image lsp)
  (boxed-letter (lsp-str lsp) (lsp-ls lsp) filled))



