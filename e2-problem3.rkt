;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname e2-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/1: Design the data RoadNetwork to represent a network of roads in a new
;           town, which contains...

;           stretches of road of a particular distance (measured in kilometers,
;           or km)

;                 |   |
;                 | 3 |
;                 |   |
;                   ↑
;
;           (example stretch of road, 3km),

;           "dead ends", where roads end

;                 -----
;                 |   |
;                   ↑
;
;           (example dead end),

;           and "T" intersections, where a road can go left or right

;               -----------
;
;               ---┐   ┌---
;                  |   |
;                    ↑
;
;           (example T-intersection).

;           Notes:
;           - Your design does NOT need to in any way visualize the road
;             network, only represent any network made up of these road types.
;           - Remember to follow all steps of the design recipe for data!
;           - Your design MUST contain the following example (written
;             and then visualized for clarity; you may create more examples if
;             you'd like but they will not be graded)...

;             A stretch 3km long, arriving at a T intersection:
;             - to the left is a dead end.
;             - to the right is a stretch 1km long, followed by another stretch
;               7km long, arriving at a T-intersection:
;               - to the left is a stretch 4km long, followed by a dead end.
;               - to the the right is a stretch 2km long, followed by a
;                 T-intersection:
;                 - to the left is a stretch 2km long, followed by a dead end.
;                 - to the right is a stretch 3km long, followed by a dead end.
       
;                                ---
;                               |   |
;                               |   |
;                               | 4 |
;                               |   |
;                 --------------┘   |
;                |       1    7     |
;                 -┐   ┌--------┐   |
;                  |   |        | 2 |
;                  | 3 |        |   |
;                  |   |     ---┘   └--
;                    ↑      | 3      2 |
;                            ----------

(define-struct stretch [pos road])

; A Stretch is a (make-stretch [PosReal RoadNetwork])
; Interpretation :
; A stretch of road where PosReal is the length in km
; and RoadNetwork is the continuation of the path

(define (stretch-temp s)
  (... (stretch-pos s) ...
       (roadnet-temp (stretch-road s)) ...))

(define-struct t [left right])

; A T-intersection is a (make-t RoadNetwork RoadNetwork)
; Interpretation :
; An intersetction where the left and right contains
; a RoadNetwork which is a continuation of the town

(define (t-temp int)
  (... (roadnet-temp (t-left int)) ...
       (roadnet-temp (t-right int)) ...))

; A RoadNetwork is one of
; - "dead end"
; - Stretch
; - T-intersection
; Interpretation :
; A representation of a network of roads in a town

(define ROAD-6 (make-stretch 2 "dead end"))
(define ROAD-7 (make-stretch 3 "dead end"))
(define T-3 (make-t ROAD-6 ROAD-7))
(define ROAD-4 (make-stretch 4 "dead end"))
(define ROAD-5 (make-stretch 2 T-3))
(define T-2 (make-t ROAD-4 ROAD-5))
(define ROAD-3 (make-stretch 7 T-2))
(define ROAD-2 (make-stretch 1 ROAD-3))
(define T-1 (make-t "dead end" ROAD-2))
(define ROAD-1 (make-stretch 3 T-1))

(define RN-1 ROAD-1)

(define (roadnet-temp rn)
  (...
   (cond
     [(string? rn) ...]
     [(stretch? rn) ... (stretch-temp rn) ...]
     [(t? rn) ... (t-temp rn) ...])))