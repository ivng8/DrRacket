;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname e1-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Your team has been tasked with building the next-generation app for Delta
; Air Lines. As a part of this massive undertaking, your job is to design
; the data to represent a single row on an international Delta flight.

; TODO 1/1: Design the data Row that has a number (e.g., 33), a passenger
;           class (either "main", "comfort+", "premium select", or
;           "delta one"), and the designation as an exit row or not.
;
;           Ensure you follow the design recipe for data!

; A Class is one of:
; - "main"
; - "comfort+"
; - "premium select"
; - "delta one"
; Interpretation: a passenger class on a plane

(define C-M "main")
(define C-C "comfort+")
(define C-P "premium select")
(define C-D "delta one")

(define (class-temp c)
  (...
   (cond
     [(string=? C-M c) ...]
     [(string=? C-C c) ...]
     [(string=? C-P c) ...]
     [(string=? C-D c) ...])))

(make-struct row [number class designation])

; A Row is a (make-row PosInt Class Boolean)
; Interpretation: A representation of a single row on an internation Delta flight
; - number is the row number
; - class is the passenger class
; - designation tells if the row is an exit row or not

(define R-1 (make-row 1 C-M #f))
(define R-5 (make-row 5 C-C #f))
(define R-10 (make-row 10 C-P #f))
(define R-15 (make-row 15 C-D #t))

(define (row-temp r)
  (... (row-number r) ...
       (class-temp (row-class r)) ...
       (row-designation r) ...))