;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname e1-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following data definition...


(define-struct puppy [name loud? rest])

; A DoggieParade is one of:
; - ":("
; - (make-puppy String Boolean DoggieParade)
; Interpretation: a sequence of doggies that is either empty (":(") or
; a puppy (with its name, whether it's loud, and the rest of the parade
; that follows.

(define DP-1 ":(")
(define DP-2 (make-puppy "Bailey" #f DP-1))
(define DP-3 (make-puppy "Spot" #t DP-2))
(define DP-4 (make-puppy "Pipen" #f DP-3))

(define (puppy-temp dp)
  (...
   (cond
     [(string? dp) ...]
     [(puppy? dp)
      (... (puppy-name dp) ...
           (puppy-loud? dp) ...
           (puppy-temp (puppy-rest dp)) ...)])))

; TODO 1/2: When you live in an apartment, you really can't have a loud
;           doggie parade. Now consider a function, apartment-friendly?,
;           that determines if the supplied parade of doggies lacks even
;           a single loud puppy. Your task: write a sufficient set of tests
;           for this function.

; (check-expect (apartment-friendly? DP-1) #t)
; (check-expect (apartment-friendly? DP-2) #t)
; (check-expect (apartment-friendly? DP-3) #f)
; (check-expect (apartment-friendly? DP-4) #f)

;           Notes:
;           - You do NOT actually have to design the function, but you can
;             if you wish (we'll ignore it).
;           - Assuming you do not write the function, we recommend that you
;             comment the check-expect statements for this problem, so that
;             they don't prevent you from running code in the next part of
;             this problem.


; TODO 2/2: Design the function parade-names, which produces a textual
;           representation of the names of the puppies in the supplied
;           parade of doggies, with sadness at the end. For example,
;           an empty doggie parade would just be sad (":("), whereas
;           one that started with Bailey, followed by Spot, would be
;           "Bailey -> Spot -> :(".

(check-expect (parade-names DP-1) ":(")
(check-expect (parade-names DP-2) "Bailey -> :(")
(check-expect (parade-names DP-3) "Spot -> Bailey -> :(")
(check-expect (parade-names DP-4) "Pipen -> Spot -> Bailey -> :(")

; parade-names : DoggieParade -> String
; Creates a textual representation of the names of the puppies
; from the suppoed parade of doggies until there are no more puppies

(define (parade-names dp)
  (cond
    [(string? dp) dp]
    [(puppy? dp)
     (string-append (puppy-name dp) " -> "
                    (parade-names (puppy-rest dp)))]))