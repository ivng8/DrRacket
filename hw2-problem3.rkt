;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let's make a SUPER simple wordle-esque game... we'll call simple wordle.

; You start the game with a blank screen, waiting for a player to type a
; valid letter (A-E, upper- or lower-case should work; cough, see problem #1);
; pressing any other key should not have any effect.

; Once a valid letter has been pressed, display that (in upper-case form) and
; wait for a key to be pressed, which could be...
;
; - backspace ("\b" in DrRacket) to go to the prior step;
; - or enter ("\r" in DrRacket) to see if they win
;
; (again, pressing any other key should not have any effect).

; Once enter has been pressed, the game is over. The player wins if they entered
; "C", but lose otherwise. If they win, they see a smiley face (":)") and the game
; returns a certain number of points; if they lose, they get a sad face (":(") and
; lose those points.

; To begin, here is a data definition you'll use for this problem...


; A SimpleWordle is one of:
; - ""
; - "A"
; - "B"
; - "C"
; - "D"
; - "E"
; - ":)"
; - ":("
; Interpretation: status of a simple wordle game, where...
; - nothing has been entered yet ("");
; - a valid character has been entered ("A"-"E"); or
; - the entry has been scored right (":)") or wrong (":(")

(define SW-EMPTY "")
(define SW-A "A")
(define SW-B "B")
(define SW-C "C")
(define SW-D "D")
(define SW-E "E")
(define SW-CORRECT ":)")
(define SW-INCORRECT ":(")

(define (sw-temp sw)
  (...
   (cond
     [(string=? sw SW-EMPTY) ...]
     [(string=? sw SW-A) ...]
     [(string=? sw SW-B) ...]
     [(string=? sw SW-C) ...]
     [(string=? sw SW-D) ...]
     [(string=? sw SW-E) ...]
     [(string=? sw SW-CORRECT) ...]
     [(string=? sw SW-INCORRECT) ...])))


; And here's an example function that uses this data definition (that might
; come in handy later!)


; simple-wordle/eval : SW -> SW
; the correct answer is only C in simple wordle

(check-expect (simple-wordle/eval SW-EMPTY) SW-INCORRECT)
(check-expect (simple-wordle/eval SW-A) SW-INCORRECT)
(check-expect (simple-wordle/eval SW-B) SW-INCORRECT)
(check-expect (simple-wordle/eval SW-C) SW-CORRECT)
(check-expect (simple-wordle/eval SW-D) SW-INCORRECT)
(check-expect (simple-wordle/eval SW-E) SW-INCORRECT)
(check-expect (simple-wordle/eval SW-CORRECT) SW-INCORRECT)
(check-expect (simple-wordle/eval SW-INCORRECT) SW-INCORRECT)

(define (simple-wordle/eval sw)
  (if (string=? sw SW-C)
      SW-CORRECT
      SW-INCORRECT))


; Now building on this work, make sure to read the following four steps
; before starting :)

; TODO 1/4: Design the function simple-wordle-done?, which accepts a SimpleWordle
;           and returns #true only if it is SW-CORRECT or SW-INCORRECT.
;
;           Hint: start with the template and then see if you can simplify!

; swd?: SW -> Boolean
; shows that the game is over once the answer is correct or incorrect

(define (swd? sw)
  (cond
    [(string=? (simple-wordle/eval sw) SW-CORRECT) #true]
    [(string=? (simple-wordle/eval sw) SW-INCORRECT) #true]
    [else #false]))


; TODO 2/4: Design the function draw-simple-wordle, which accepts a SimpleWordle
;           and produces a visualization of it. Empty should just be a blank
;           image; A-E should just have the letter in the middle; and the
;           (in)correct should show the appropriate face.
;
;           Hint: after making your tests, see if you can detect a pattern to
;           produce a simple visualization function.

; dsw: SW -> Image
; provides the visualization of a SW

(define BACKGROUND (square 100 "solid" "white"))

(define (dsw sw)
  (cond
    [(string=? sw SW-EMPTY) (overlay (text " " 50 "blue") BACKGROUND)]
    [(string=? sw SW-A) (overlay (text "A" 50 "blue") BACKGROUND)]
    [(string=? sw SW-B) (overlay (text "B" 50 "blue") BACKGROUND)]
    [(string=? sw SW-C) (overlay (text "C" 50 "blue") BACKGROUND)]
    [(string=? sw SW-D) (overlay (text "D" 50 "blue") BACKGROUND)]
    [(string=? sw SW-E) (overlay (text "E" 50 "blue") BACKGROUND)]
    [(string=? sw SW-CORRECT) (overlay (text ":)" 50 "blue") BACKGROUND)]
    [(string=? sw SW-INCORRECT) (overlay (text ":(" 50 "blue") BACKGROUND)]))

; TODO 3/4: Uncomment the following function - you are not allowed to change it, but
;           rather you must design the two helper functions it references.
;           The first (key-simple-wordle/empty) produces a SimpleWordle based upon
;           the key pressed when the player is facing an empty screen.
;           The second (key-simple-wordle/letter) produces a SimpleWord based upon
;           the key pressed when the player has already entered a valid guess.
;
;           Hint: start by understanding the supplied tests, then, for each helper,
;           work through the steps of the design recipe one at a time.


; key-simple-wordle : SW KeyEvent -> SW
; based upon the current state, handle a key press and produce the new state

(check-expect (key-simple-wordle SW-EMPTY "a") SW-A)
(check-expect (key-simple-wordle SW-EMPTY "B") SW-B)
(check-expect (key-simple-wordle SW-EMPTY "x") SW-EMPTY)
(check-expect (key-simple-wordle SW-EMPTY "\r") SW-EMPTY)
(check-expect (key-simple-wordle SW-EMPTY "\b") SW-EMPTY)

(check-expect (key-simple-wordle SW-A "a") SW-A)
(check-expect (key-simple-wordle SW-A "B") SW-A)
(check-expect (key-simple-wordle SW-A "x") SW-A)
(check-expect (key-simple-wordle SW-A "\r") SW-INCORRECT)
(check-expect (key-simple-wordle SW-A "\b") SW-EMPTY)

(check-expect (key-simple-wordle SW-C "a") SW-C)
(check-expect (key-simple-wordle SW-C "B") SW-C)
(check-expect (key-simple-wordle SW-C "x") SW-C)
(check-expect (key-simple-wordle SW-C "\r") SW-CORRECT)
(check-expect (key-simple-wordle SW-C "\b") SW-EMPTY)

(check-expect (key-simple-wordle SW-CORRECT "a") SW-CORRECT)
(check-expect (key-simple-wordle SW-CORRECT "B") SW-CORRECT)
(check-expect (key-simple-wordle SW-CORRECT "x") SW-CORRECT)
(check-expect (key-simple-wordle SW-CORRECT "\r") SW-CORRECT)
(check-expect (key-simple-wordle SW-CORRECT "\b") SW-CORRECT)

(check-expect (key-simple-wordle SW-INCORRECT "a") SW-INCORRECT)
(check-expect (key-simple-wordle SW-INCORRECT "B") SW-INCORRECT)
(check-expect (key-simple-wordle SW-INCORRECT "x") SW-INCORRECT)
(check-expect (key-simple-wordle SW-INCORRECT "\r") SW-INCORRECT)
(check-expect (key-simple-wordle SW-INCORRECT "\b") SW-INCORRECT)

(define (key-simple-wordle sw ke)
  (cond
    [(string=? sw SW-EMPTY) (key-simple-wordle/empty ke)]
    [(or (string=? sw SW-A)
         (string=? sw SW-B)
         (string=? sw SW-C)
         (string=? sw SW-D)
         (string=? sw SW-E)) (key-simple-wordle/letter sw ke)]
    [(or
      (string=? sw SW-CORRECT)
      (string=? sw SW-INCORRECT)) sw]))

; key-simple-word/empty : KeyEvent -> SW
; empty state that is ready for SW guess

(check-expect (key-simple-wordle/empty "a") SW-A)
(check-expect (key-simple-wordle/empty "x") SW-EMPTY)

(define (key-simple-wordle/empty ke)
  (cond
    [(key=? (string-upcase ke) "A") SW-A]
    [(key=? (string-upcase ke) "B") SW-B]
    [(key=? (string-upcase ke) "C") SW-C]
    [(key=? (string-upcase ke) "D") SW-D]
    [(key=? (string-upcase ke) "E") SW-E]
    [else SW-EMPTY])) 

; key-simple-wordle/letter : SW KeyEvent -> SW
; guessing state is can go back to empty state or result state

(check-expect (key-simple-wordle/letter "C" "\r") SW-CORRECT)
(check-expect (key-simple-wordle/letter "C" "\b") SW-EMPTY)

(define (key-simple-wordle/letter sw ke)
  (cond
    [(key=? ke "\r") (simple-wordle/eval sw)]
    [(key=? ke "\b") SW-EMPTY]
    [else sw]))

; TODO 4/4: Write the code for the function play-simple-wordle, which uses big-bang
;           to play a game of SimpleWordle. When calling the function, the player
;           provides the points to return if they win (or negative that value if
;           they lose the game). You have been provided the signature and purpose,
;           as well as a commented-out test; you just need to write the function
;           code in the middle.
;
;           Hint: you have all the handlers you need, so your decisions to make...
;           - what is the initial state (i.e., how does your game begin?)
;           - what events matter in this game, and what handlers do you have for them?
;           - what will big-bang return, and how do you combine that with the points
;             you've been supplied?



; play-simple-wordle : Nat -> Nat
; returns the supplied points if the game is won, otherwise negative

(define (play-simple-wordle x)
  (if (string=? (big-bang SW-EMPTY
                  [to-draw dsw]
                  [on-key key-simple-wordle]) SW-CORRECT) (+ 0 x) (- 0 x)))

; uncomment to play a game and check that the output is valid
; (must come AFTER your code)

(define SIMPLE-WORDLE-RESULT (play-simple-wordle 7))

(check-expect (or (= SIMPLE-WORDLE-RESULT 7)
                  (= SIMPLE-WORDLE-RESULT -7))
              #true)


