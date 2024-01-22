;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let's think about what goes into designing notifications for a mobile device.
;
; Consider the following data definitions...


(define-struct info [app message])

; An InfoMessage is a (make-info String String)
; Interpretation: a message from an app

(define INFO-1 (make-info "Instagram" "Hello"))
(define INFO-2 (make-info "Snapchat" "Bye"))
(define INFO-3 (make-info "Facebook" "What's up"))

(define (info-temp info)
  (... (info-app info) ...
       (info-message info) ...))

(define-struct badge [app num])

; A Badge is a (make-badge String Nat)
; Interpretation: a numeric indicator for an app

(define BADGE-1 (make-badge "Instagram" 1))
(define BADGE-2 (make-badge "Snapchat" 2))
(define BADGE-3 (make-badge "Facebook" 3))

(define (badge-temp badge)
  (... (badge-app badge) ...
       (badge-num badge) ...))

(define-struct confirm [app yestxt notxt])

; A Confirmation is a (make-confirm String String String)
; Interpretation: a yes/no question from an app, with
; associated text to display for each option

(define CONFIRM-1 (make-confirm "Instagram" "Accept" "Deny"))
(define CONFIRM-2 (make-confirm "Snapchat" "Yes" "No"))
(define CONFIRM-3 (make-confirm "Facebook" "Ok" "Cancel"))

(define (conf-temp confirm)
  (... (confirm-app confirm) ...
       (confirm-yestxt confirm) ...
       (confirm-notxt confirm) ...))

; A Notification is one of the following
; - info
; - badge
; - confirm
; Interpretation: a type of notification from an app

(define NOTIF-1 INFO-1)
(define NOTIF-2 BADGE-2)
(define NOTIF-3 CONFIRM-3)

(define (notif-temp notif)
  (...
   (cond
     [(info? notif) (info-temp notif)]
     [(badge? notif) (badge-temp notif)]
     [(confirm? notif) (confirm-temp notif)])))

; TODO 1/2: Complete the design recipe for InfoMessage,
;           Badge, and Confirmation. You should come up
;           with reasonable examples, but are welcome
;           to be creative :)




; TODO 2/2: Design the data type Notification, which represents
;           a single notification that could be of any of the
;           types described above.




