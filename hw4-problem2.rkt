;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now let's think about what goes into making a particularly useful app, Weather!
;
; Consider the following data definition:


(define-struct cloudy [morn? eve?])
(define-struct rain [chance])
(define-struct snow [inches])

; A Prediction is one of:
; - "sunny"
; - (make-cloudy Boolean Boolean)
; - (make-rain Nat[1, 100])
; - (make-snow Nat)
; Intepretation: weather prediction, either...
; - Sunny!
; - Cloudy (either in the morning, evening, both, or unsure)
; - Raining (with provided % chance as 1-100)
; - Snow (with provided accumulation)

(define PRED-1 "sunny")
(define PRED-2 (make-cloudy #t #f))
(define PRED-3 (make-rain 75))
(define PRED-4 (make-snow 3))

(define (pred-temp pred)
  (...
   (cond
     [(string? pred) ...]
     [(cloudy? pred) ...]
     [(rain? pred) ...]
     [(snow? pred) ...])))

; weather : Prediction -> String
; Creates a weather announcement based off an inputted prediction

(check-expect (weather PRED-1) "It's going to be sunny!")
(check-expect (weather PRED-2) "It's cloudy in the morning.")
(check-expect (weather PRED-3) "There's a 75% chance of rain.")
(check-expect (weather PRED-4) "It's going to snow, with 3 inches on the ground.")

(define (weather pred)
  (cond
    [(string? pred) "It's going to be sunny!"]
    [(cloudy? pred)
     (cond
       [(and (boolean=? #t (cloudy-morn? pred)) (boolean=? #t (cloudy-eve? pred)))
        "It's cloudy in the morning and evening."]
       [(and (boolean=? #t (cloudy-morn? pred)) (boolean=? #f (cloudy-eve? pred)))
        "It's cloudy in the morning."]
       [(and (boolean=? #f (cloudy-morn? pred)) (boolean=? #t (cloudy-eve? pred)))
        "It's cloudy in the evening."]
       [(and (boolean=? #f (cloudy-morn? pred)) (boolean=? #f (cloudy-eve? pred)))
        "It's not cloudy today."])]
    [(rain? pred) (string-append "There's a "
                                 (number->string (rain-chance pred))
                                 "% chance of rain.")]
    [(snow? pred) (string-append "It's going to snow, with "
                                 (number->string (snow-inches pred))
                                 " inches on the ground.")]))
  

; TODO 1/2: Complete the data design recipe for Prediction.



; TODO 2/2: Design the function announcement, which given
;           a prediction (e.g., "sunny"), produces a short
;           text announcement to display (e.g., "It's going
;           to be sunny!").

;           Some other example announcements include:
;           - "It's going to be cloudy in the morning."
;           - "There's a 60% chance of rain."
;           - "It's going to snow, with 2 inches on the ground."




