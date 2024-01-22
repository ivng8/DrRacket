;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw5-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You are going to make yourself a useful interactive app: flash cards
; (https://en.wikipedia.org/wiki/Flashcard).

; To begin, consider the following data definition...


(define-struct flashcard [front back])

; A FlashCard is a (make-flashcard String String)
; Interpretation: the front and back of a card


; TODO 1/4: Complete the design recipe for FlashCard.

(define FLASH-1 (make-flashcard "Hello" "Bye"))
(define FLASH-2 (make-flashcard "Good" "Bad"))
(define FLASH-3 (make-flashcard "Small" "Big"))

(define (flashcard-temp f)
  (... (flashcard-front f) ...
       (flashcard-back f) ...))

; Now a single flash card wouldn't be super useful, and so...

; TODO 2/4: Design ListOfFlashCard (LoFC) to support an arbitrarily sized
;           sequence of flash cards. Importantly...
;           - These should be proper lists (i.e., using cons and '()).
;           - Make sure to give yourself a few example lists, of different sizes;
;             hopefully they are useful in your classes!
;           - Remember that your LoFC template should reflect that your list
;             elements are themselves designed types (FlashCard).

; A ListOfFlashCard (LoFC) is one of:
; - '()
; - (cons FlashCard ListOfFlashCard)
; Interpretation: A list of flashcards

(define LOFC-0 '())
(define LOFC-1 (cons FLASH-1 LOFC-0))
(define LOFC-2 (cons FLASH-2 LOFC-1))
(define LOFC-3 (cons FLASH-3 LOFC-2))

(define (list-temp l)
  (...
   (cond
     [(empty? l) ...]
     [(cons? l)
      (... (first l) ...
           (list-temp (rest l)) ...)])))

; Now, for practice...

; TODO 3/4: Design the function has-text?, which determines if a list of flash
;           cards contains any card that contains a supplied text.
;
;           Hint: the string-contains? function is very useful for determining
;           if one string contains another :)

; has-text? : ListOfFlashCard String -> Boolean
; determines if any card in a list has
; a supplied text

(check-expect (has-text? LOFC-0 "Hey") #f)
(check-expect (has-text? LOFC-1 "Hello") #t)
(check-expect (has-text? LOFC-2 "Bye") #t)
(check-expect (has-text? LOFC-3 "Hey") #f)

(define (has-text? l s)
  (cond
    [(empty? l) #f]
    [(cons? l)
     (or
      (string-contains? s (flashcard-front (first l)))
      (string-contains? s (flashcard-back (first l)))
      (has-text? (rest l) s))]))

; Finally, let's put this list to use :)

; TODO 4/4: Design the program go-cards, which helps you study with a supplied list
;           of cards. It starts on the first card and then flips it when a key is
;           pressed, and then goes to the front of the next card when another key is
;           pressed. The program should end when the last card has been flipped, and
;           the go-cards function should return how many cards were in the original
;           list. Some hints...
;           - To get you started, you have been supplied the data definition of a
;             way to represent the state of the program (don't forget to uncomment
;             the structure definition and finish the design recipe for data!).
;           - The return value of this function is a bit challenging, since the list
;             you get at the end is empty! So uncomment the code we've given you below,
;             but to understand: you can *add* the length of the originally supplied
;             list to that of the (empty) final list and still get the right answer :)
;           - Be sure to follow the templates for all your data, which will typically
;             entail helpers for the FS, the LoFC, and the FC.
;           - As long as the program operates as described, you are welcome to make it
;             look as simple or as creative as you would like - we hope it helps you
;             in your classes!! :)


(define-struct fs [cards front?])

; A FlashState (FS) is a (make-fs LoFC Boolean)
; Interpretation: a list of cards, and whether
; the front is face up

(define (flash-temp f)
  (... (fs-cards f) ...
       (fs-front? f) ...))

; go-cards : LoFC -> Nat
; displays the cards in sequence (flip via key),
; returning the number of cards


(check-expect (go-cards LOFC-0) 0)
(check-expect (go-cards LOFC-1) 1)
(check-expect (go-cards LOFC-2) 2)
(check-expect (go-cards LOFC-3) 3)



(define (go-cards lofc)
  (+
   (length lofc)
   (length (fs-cards
            (big-bang (make-fs lofc #t)
              [to-draw draw-fs]
              [on-key flip-fs]
              [stop-when done-fs?])))))


(define BACKGROUND (rectangle 100 100 "solid" "white"))

; draw-fs : FlashState -> Image
; visualizes the flashcards

(check-expect (draw-fs (make-fs LOFC-3 #t))
              (overlay (text "Small" 30 "black") BACKGROUND))
(check-expect (draw-fs (make-fs LOFC-3 #f))
              (overlay (text "Big" 30 "black") BACKGROUND))
(check-expect (draw-fs (make-fs LOFC-2 #t))
              (overlay (text "Good" 30 "black") BACKGROUND))
(check-expect (draw-fs (make-fs LOFC-2 #f))
              (overlay (text "Bad" 30 "black") BACKGROUND))
(check-expect (draw-fs (make-fs LOFC-1 #t))
              (overlay (text "Hello" 30 "black") BACKGROUND))
(check-expect (draw-fs (make-fs LOFC-1 #f))
              (overlay (text "Bye" 30 "black") BACKGROUND))

(define (draw-fs f)
  (if (fs-front? f)
      (overlay (text (flashcard-front (first (fs-cards f))) 30 "black") BACKGROUND)
      (overlay (text (flashcard-back (first (fs-cards f))) 30 "black") BACKGROUND)))

; flip-fs : FlashState KeyEvent -> FlashState
; flips the card or goes to next card

(check-expect (flip-fs (make-fs LOFC-3 #t) "p") (make-fs LOFC-3 #f))
(check-expect (flip-fs (make-fs LOFC-3 #f) "f") (make-fs LOFC-2 #t))
(check-expect (flip-fs (make-fs LOFC-2 #t) "d") (make-fs LOFC-2 #f))
(check-expect (flip-fs (make-fs LOFC-2 #f) "s") (make-fs LOFC-1 #t))
(check-expect (flip-fs (make-fs LOFC-1 #t) "h") (make-fs LOFC-1 #f))

(define (flip-fs f ke)
  (if (fs-front? f)
      (make-fs (fs-cards f) #f)
      (make-fs (rest (fs-cards f)) #t)))

; done-fs? : FlashState -> Boolean
; is the last card flipped?

(check-expect (done-fs? (make-fs LOFC-3 #t)) #f)
(check-expect (done-fs? (make-fs LOFC-0 #t)) #t)

(define (done-fs? f)
  (empty? (fs-cards f)))
