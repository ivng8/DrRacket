;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following data definitions & interpretations...


(define-struct address [num st city us-state zip])

; An Address is a (make-address Nat String String String Nat)
; - num is the number of the building on the street
; - st is the name of the street
; - city is the city the building is in
; - us-state is the state the city is in
; - zip is the zipcode of the building
; Interpretation: a US address

(define MY (make-address 8811 "Bay-Parkway" "NYC" "NY" 11214))
(define NEU (make-address 100 "Bacon-Street" "Boston" "MAS" 10990))

(define (address-temp address)
  (...
   (cond
     [...]
     [...])))

(define-struct student [first last nuid local perm])

; An NUStudent is a (make-student String String PositiveNumber Address Address)
; - first is the student's first name
; - last is the student's last name
; - nuid is the student's NUID
; - local is the student's local address
; - perm is the student's permanent address
; Interpretation: a Northeastern student

(define ME (make-student "Ivan" "Ng" 002991988 NEU MY))

(define (student-temp student)
  (...
   (cond
     [...]
     [...])))

; TODO 1/3: complete the data design recipe for Address and NUStudent




; TODO 2/3: Design the function student-email which takes an NUStudent and
;           produces a string representing that student’s email address.
;           For simplicity we will say that a student’s email address is always
;           their last name (all lowercase),  followed by a period, followed
;           by the first initial of their first name (also lowercase; you can
;           assume this exists), and finished with "@northeastern.edu".

; student-email : NUStudent -> String
; Takes an NUStudent and outputs his/her email address

(check-expect (student-email ME) "ng.i@northeastern.edu")

(define (student-email student)
  (string-append
   (string-downcase (student-last student))
   "."
   (string-downcase (substring (student-first student) 0 1))
   "@northeastern.edu"))


; TODO 3/3: Design the function update-perm-zipcode which takes an NUStudent
;           and a natural number, representing the new zip code of the person,
;           and updates their permanent address to have that zip code.
;
;           Be sure to follow the template!

; update-perm-zipcode : NUStudent Real -> NUStudent
; Updates a NUStudents zip code in their permanent address

(define MY-NEW (make-address 8811 "Bay-Parkway" "NYC" "NY" 11215))
(define ME-NEW (make-student "Ivan" "Ng" 002991988 NEU MY-NEW))

(check-expect (update-perm-zipcode ME 11215) ME-NEW)

(define (update-perm-zipcode student x)
  (make-student
   (student-first student)
   (student-last student)
   (student-nuid student)
   (student-local student)
   (make-address
    (address-num (student-perm student))
    (address-st (student-perm student))
    (address-city (student-perm student))
    (address-us-state (student-perm student))
    x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note: to receive full credit, submit as much as you complete - you do NOT
;       have to finish all parts in lab.


; You are to design a program text-mover to display and manipulate text on a
; background. Your program should accept some phrase to show, as well as initial
; location and color (we only support three: red, black, or purple) - you should
; then display the phrase on the screen as described.

; When the user presses a mouse button, the program should move the text to the
; location that they clicked. When the user presses any key on the keyboard, the
; program should rotate colors.

; Here is our suggested plan for this program...

; 1. Design the text-mover function - think through the arguments to the
;    function, how you will represent the world state, and what handlers
;    you need to support.
;
;    - Hint A: since your state has multiple parts that change, you'll need a
;              structure to hold them, but the parts themselves might also be new.
;    - Hint B: you've been provided some data definitions below that will be quite
;              useful.

; 2. Finish designing the data from #1; think ahead to make examples that are
;    useful for testing such operations as changing location and color.

; 3. Design your to-draw handler, making use of the template(s) you 
;    designed in #2.

; 4. Design your remaining handler(s), again following the appropriate template(s).
;
;    - Hint #1: for the mouse, you'll want to respond only to the "button-up"
;               event, which you can check using the mouse=? function. Here's code
;               to get you started...

(define (mouse-handler state x y me)
  (if (mouse=? me "button-up")
      ...
      ...))

;    - Hint #2: make sure to follow your templates, which may involve breaking 
;               the handlers into helper functions.


; TODO 1/1: Design the text-mover World program!

(define BACKGROUND (square 500 "solid" "white"))

; A Position is a (make-posn Real Real)
; Interpretation: a 2D location

(define CENTER (make-posn 250 250))

; A RedBlackPurple (RBP) is one of:
; - "red"
; - "black"
; - "purple"
; Interpretation: available font colors

(define r "red")
(define b "black")
(define p "purple")

(define-struct tm [str pos col])

; A TextMover (TM) is a (make-tm String Position RBP)
; - str is the text to be displayed
; - pos is the location of the text
; - col is the color of the text
; Interpretation: all the information needed for the text-mover program.

(define initial-state (make-tm "NEU" CENTER r))

; draw-image : TM -> Image
; draws the overall image

(define (draw-image tm)
  (place-image
   (text (tm-str tm) 36 (tm-col tm))
   (posn-x (tm-pos tm))
   (posn-y (tm-pos tm))
   BACKGROUND))

; color-change : TM KeyEvent -> TM
; changes the color of the text upon key inputs

(define (color-change tm ke)
  (cond
    [(and (string=? (tm-col tm) r) (key-event? ke))
     (make-tm (tm-str tm) (tm-pos tm) b)]
    [(and (string=? (tm-col tm) b) (key-event? ke))
     (make-tm (tm-str tm) (tm-pos tm) p)]
    [(and (string=? (tm-col tm) p) (key-event? ke))
     (make-tm (tm-str tm) (tm-pos tm) r)]))

; loc-change : TM MouseEvent -> TM
; changes the location of the text upon mouse inputs

(define (loc-change tm x y me)
  (if (mouse=? me "button-up")
      (make-tm (tm-str tm) (make-posn x y) (tm-col tm))
      tm))

(big-bang initial-state
  [to-draw draw-image]
  [on-key color-change]
  [on-mouse loc-change])
