;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname e2-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following data definition...


(define-struct student [name interests])

; A Student is a (make-student String [List-of String])
; Interpretation: a student and their interests

(define ST-ALICE (make-student "alice" (list "table tennis" "politics" "dance")))
(define ST-BOB (make-student "bob" (list "table tennis" "history" "cooking")))
(define ST-CHRIS (make-student "chris" (list "table tennis" "politics")))


; TODO 1/1: Finish designing the function viable-clubs that accepts a list of
;           potential club topics, a list of students, and a threshold of
;           interest (as in, at least how many students must be interested in a
;           topic for it to be a viable club) and produces a list of the club
;           topics (in the same order they were initially supplied) that are
;           viable.
;
;           For example, given the student examples above, and the potential
;           topics of table tennis, politics, dance, history, cooking, and
;           hiking, all the topics meet the threshold of 0, all but hiking
;           have at least one student interested, only table tennis and
;           politics meet the threshold of 2 students, only table tennis meets
;           the threshold of 3 students, and none of them meet the threshold of
;           4 students (since there are only 3 students!).
;
;           You have been supplied a signature, purpose, and sufficient tests
;           for this function. You are responsible for writing well-designed
;           code to solve this problem, making appropriate use of abstractions
;           and following the design recipe for any helpers.


; viable-clubs : [List-of String] [List-of Student] Nat -> [List-of String]
; produces a list of those supplied club ideas that have at least a supplied
; number of the supplied students with that topic as an interest

(check-expect (viable-clubs '() '() 2) '())

(check-expect (viable-clubs (list "table tennis" "dance") '() 1) '())

(check-expect (viable-clubs '() (list ST-ALICE) 1) '())

(check-expect (viable-clubs (list "table tennis" "politics" "dance"
                                  "history" "cooking" "hiking")
                            (list ST-ALICE ST-BOB ST-CHRIS)
                            0)
              (list "table tennis" "politics" "dance"
                    "history" "cooking" "hiking"))

(check-expect (viable-clubs (list "table tennis" "politics" "dance"
                                  "history" "cooking" "hiking")
                            (list ST-ALICE ST-BOB ST-CHRIS)
                            1)
              (list "table tennis" "politics" "dance"
                    "history" "cooking"))

(check-expect (viable-clubs (list "table tennis" "politics" "dance"
                                  "history" "cooking" "hiking")
                            (list ST-ALICE ST-BOB ST-CHRIS)
                            2)
              (list "table tennis" "politics"))

(check-expect (viable-clubs (list "table tennis" "politics" "dance"
                                  "history" "cooking" "hiking")
                            (list ST-ALICE ST-BOB ST-CHRIS)
                            3)
              (list "table tennis"))

(check-expect (viable-clubs (list "table tennis" "politics" "dance"
                                  "history" "cooking" "hiking")
                            (list ST-ALICE ST-BOB ST-CHRIS)
                            4)
              '())

(define (viable-clubs loi los nat)
  (local [; recruit : String -> Nat]
          ; Takes a topic of interest and gives
          ; the amount of students with that interest
          (define (recruit int loz)
            (cond
              [(empty? loz) 0]
              [(cons? loz)
               (+
                (if (ormap
                     (λ (x) (string=? x int))
                     (student-interests (first loz)))
                    1 0)
                (recruit int (rest loz)))]))]
    (filter (λ (y) (>= (recruit y los) nat)) loi)))

