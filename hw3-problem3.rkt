;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recently a new website (https://neal.fun/design-the-next-iphone/) was made
; to allow you to design the next iPhone by mixing & matching features.
; Let's make a simple version of this as a World program :)

; TODO 1/3: Design the data PhoneModel that allows you to represent a set
;           of (at least) four enumerated models. You are welcome to be
;           creative, but an example set of options could be "Small",
;           "Regular", "Max", or "Fold"

; PhoneModel (PM) is one of the following
; - "Small"
; - "Regular"
; - "Max"
; - "Fold"
; Interpretation: represents a possible attribute of the phone
(define PM-S "Small")
(define PM-R "Regular")
(define PM-M "Max")
(define PM-F "Fold")

; TODO 2/3: Now design the data PhoneCompany that allows you to represent a set
;           of (at least) three enumerated company names, such as "Apple",
;           "Google", "Amazon".

; PhoneCompany (PC) is one of the follwing
; - "Apple"
; - "Google"
; - "Amazon"
; Interpretation: represents the company behind the phone

(define PC-APPLE "Apple")
(define PC-GOOGLE "Google")
(define PC-AMAZON "Amazon")

; TODO 3/3: Now design a World program that allows someone to interactively see all
;           combinations of models and companies.

(define BACKGROUND (square 500 "solid" "white"))

(define-struct phone [PM PC])

(define phone-SAP (make-phone PM-S PC-APPLE))
(define phone-SGO (make-phone PM-S PC-GOOGLE))
(define phone-SAM (make-phone PM-S PC-AMAZON))
(define phone-RAP (make-phone PM-R PC-APPLE))
(define phone-RGO (make-phone PM-R PC-GOOGLE))
(define phone-RAM (make-phone PM-R PC-AMAZON))
(define phone-MAP (make-phone PM-M PC-APPLE))
(define phone-MGO (make-phone PM-M PC-GOOGLE))
(define phone-MAM (make-phone PM-M PC-AMAZON))
(define phone-FAP (make-phone PM-F PC-APPLE))
(define phone-FGO (make-phone PM-F PC-GOOGLE))
(define phone-FAM (make-phone PM-F PC-AMAZON))

; phone-image: phone -> Image
; draws the image of the phone

(define (phone-image phone)
  (overlay
   (overlay
    (cond
      [(string=? (phone-PC phone) PC-APPLE) (text PC-APPLE 15 "black")]
      [(string=? (phone-PC phone) PC-GOOGLE) (text PC-GOOGLE 15 "black")]
      [(string=? (phone-PC phone) PC-AMAZON) (text PC-AMAZON 15 "black")])
    (cond
      [(string=? (phone-PM phone) PM-S)
       (overlay (rectangle 80 180 "solid" "white") (rectangle 100 200 "solid" "black"))]
      [(string=? (phone-PM phone) PM-R)
       (overlay (rectangle 130 230 "solid" "white") (rectangle 150 250 "solid" "black"))]
      [(string=? (phone-PM phone) PM-M)
       (overlay (rectangle 180 280 "solid" "white") (rectangle 200 300 "solid" "black"))]
      [(string=? (phone-PM phone) PM-F)
       (overlay (add-line (rectangle 230 330 "solid" "white") 115 0 115 330 "black")
                (rectangle 250 350 "solid" "black"))]))
   BACKGROUND))
  

; phone-type: KeyEvent phone -> phone
; toggles the company and type of the phone

(check-expect (phone-type phone-SAP "m") phone-RAP)
(check-expect (phone-type phone-SAP "c") phone-SGO)
(check-expect (phone-type phone-RAM "m") phone-MAM)
(check-expect (phone-type phone-RAM "c") phone-RAP)

(define (phone-type phone ke)
  (make-phone
   (cond
     [(and (key=? ke "m") (string=? (phone-PM phone) PM-S)) PM-R]
     [(and (key=? ke "m") (string=? (phone-PM phone) PM-R)) PM-M]
     [(and (key=? ke "m") (string=? (phone-PM phone) PM-M)) PM-F]
     [(and (key=? ke "m") (string=? (phone-PM phone) PM-F)) PM-S]
     [else (phone-PM phone)])
   (cond
     [(and (key=? ke "c") (string=? (phone-PC phone) PC-APPLE)) PC-GOOGLE]
     [(and (key=? ke "c") (string=? (phone-PC phone) PC-GOOGLE)) PC-AMAZON]
     [(and (key=? ke "c") (string=? (phone-PC phone) PC-AMAZON)) PC-APPLE]
     [else (phone-PC phone)])))

(big-bang phone-SAP
  [to-draw phone-image]
  [on-key phone-type])

;           Notes:
;           - You will need to design data to represent the current combination of
;             model/company, which should be a structure. Your program can accept
;             the starting value and should return the last when the window is closed.
;           - When the "m" keyboard key is pressed, you should proceed to the next
;             model (e.g., "Small" -> "Regular" -> "Max" -> "Fold" -> "Small"...);
;             the "c" key should similarly work for companies.
;           - Your drawing function should overlay some visualization of the company
;             (e.g., could be just the text, or a pretty logo) on top of a
;             visualization of the model (e.g., could differ in size, camera, ...).
;           - Be sure to follow the templates, which will guide you as to when to
;             create data-specific helper functions!!


