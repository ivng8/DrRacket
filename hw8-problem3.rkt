;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; IMPORTANT:
; 1. The functions that you design for this problem *must* use the ISL list
;    abstraction(s); you MAY NOT use recursion: doing so will lead you to get
;    no code credit for the function :(
;
; 2. The planning part of this problem is in the SAME file used for ALL parts
;    of Homework 8.


; Your task in this problem is to help fishing boat skippers adhere to strict
; rules about fishing in the United States. In particular, catching and keeping
; a fish that is below a certain size can result in a hefty fine. However, these
; rules don't apply to any fish designed as an "invasive species." (In fact, in
; many locations, the act of releasing these fish back into the wild carries its
; own fine!)

; For a fishing boat skipper to know that they are following the above rules for
; fish to release, they must keep track their fish and at every point in time on
; the trip, they need to be able to determine which non-invasive fish in their
; catch are above the minimum size restriction (we'll use 8 inches). For this
; purpose, a catch is represented with the following data definition:


(define-struct fish [species length-in-inches])

; A Fish is a (make-fish String PosNum)
; Interpretation: name of fish and length in inches.

(define FISH-1 (make-fish "Carp" 20))
(define FISH-2 (make-fish "Guppy" 1))
(define FISH-3 (make-fish "Lake Trout" 8))
(define FISH-4 (make-fish "Snakehead" 10))

(define (fish-temp f)
  (... (fish-species f) ...
       (fish-length-in-inches f) ...))

; viable-fish : [List-of Fish] [List-of String] -> [List-of Fish]

(check-expect (viable-fish (list)
                           (list "Carp"))
              (list))
(check-expect (viable-fish (list FISH-1 FISH-2 FISH-3 FISH-4)
                           (list "Carp"))
              (list FISH-4))

(check-expect (viable-fish (list FISH-1 FISH-2 FISH-3 FISH-4)
                           (list "Snakehead"))
              (list FISH-1))

(define (viable-fish lof los)
  (local [; valid-fish? Fish -> Boolean
          ; takes a fish and returns true
          ; if it is both longer than 8 inches
          ; and not an invasive species
          (define (valid-fish? f)
            (and
             (> (fish-length-in-inches f) 8)
             (andmap
              (λ (x) (not (string=? x (fish-species f))))
              los)))]
    (filter
     valid-fish?
     lof)))


; Your task is to design the function viable-fish, which takes in a list of
; caught fish and a list of names of invasive species (represented as strings)
; and returns a list, in the same order as the input, of the non-invasive fish
; that are larger than 8 inches.


; TODO 1/2: Plan your solution, using the planning interface described on the
;           canvas page for this assignment. ALL PLANNING FOR THIS HW WILL BE
;           DONE IN THE SAME PLACE, AND SUBMITTED TOGETHER.

; TODO 2/2: Design the function viable-fish using the ISL list abstractions.
;           YOUR CODE SHOULD NOT USE ANY RECURSION.


