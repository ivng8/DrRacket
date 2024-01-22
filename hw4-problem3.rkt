;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now let's think about what goes into making an app that works with purchase
; receipts.
;
; Consider the following data definition:


(define-struct item [desc qty unit sale? next])

; A ReceiptItem is one of:
; - "nothing"
; - (make-item String Nat PosReal Boolean ReceiptItem)
; Intepretation: either end of the receipt or
; an item's description, quantity purchased,
; unit price (in $), whether it was on sale,
; and the next item on the receipt

(define EMPTY "nothing")
(define GROC-R (make-item "box of cereal" 1 4.28 #f
                          (make-item "apple" 2 1.67 #t EMPTY)))
(define COMP-R (make-item "RaspberryPi" 2 32 #t
                          (make-item "monitor" 1 135 #f
                                     (make-item "wireless touch keyboards" 2 27 #f EMPTY))))
(define COMP-R2 (make-item "RaspberryPi" 2 101 #t
                           (make-item "monitor" 1 135 #f
                                      (make-item "wireless touch keyboards" 2 27 #f EMPTY))))

(define (receipt-temp ri)
  (...
   (cond
     [(string? ri) ...]
     [(item? ri) ...]
     [(item-desc ri) ...]
     [(item-qty ri) ...]
     [(item-unit ri) ...]
     [(item-sale? ri) ...]
     [(reciept-temp (item-next ri)) ...])))


; TODO 1/4: Complete the data design recipe for ReceiptItem.
;           You *must* have examples that (at least) represent the following
;           three receipts...
;           - An empty receipt
;           - A grocery receipt...
;             (1 box of cereal, $4.28),
;             (2 apples on sale, $1.67 each)
;           - A computer invoice...
;             (2 RaspberryPi on sale, $32 each),
;             (1 monitor, $135),
;             (2 wireless touch keyboards, $27 each)

; TODO 2/4: Design the function total-cost, which calculates the total cost
;           of a receipt. For instance, the empty receipt is 0; the grocery
;           is (1 x 4.28) + (2 x 1.67) = 7.62; and the computer receipt is
;           (2 x 32) + (1 x 135) + (2 x 27) = 253.

; total-cost : ReceiptItem -> Real
; calculates total cost of the receipt

(check-expect (total-cost EMPTY) 0)
(check-expect (total-cost GROC-R) 7.62)
(check-expect (total-cost COMP-R) 253)

(define (total-cost ri)
  (if
   (string? ri) 0
   (+ (* (item-qty ri) (item-unit ri)) (total-cost (item-next ri)))))

; TODO 3/4: Design the function any-sale?, which determines if any item in the
;           receipt is on sale. For example, the empty receipt does not have
;           any sale items, but both other examples do.

; any-sale? : RecieptItem -> Boolean
; determines if any item in the receipt is on sale

(check-expect (any-sale? EMPTY) #f)
(check-expect (any-sale? GROC-R) #t)
(check-expect (any-sale? COMP-R) #t)

(define (any-sale? ri)
  (if
   (string? ri) #f
   (if (boolean=? #t (item-sale? ri)) #t (any-sale? (item-next ri)))))

; TODO 4/4: Design the function expensive, which produces a new receipt that only
;           contains items that are greater than $100 (unit cost). For example,
;           both the empty and grocery receipts would produce empty receipts,
;           whereas the computer receipt would produce a new list only containing
;           the monitor.

; new-receipt : ReceiptItem -> ReceiptItem
; Creates a new receipt with only items that are greater than $100

(check-expect (new-receipt EMPTY) EMPTY)
(check-expect (new-receipt GROC-R) EMPTY)
(check-expect (new-receipt COMP-R) (make-item "monitor" 1 135 #f EMPTY))
(check-expect (new-receipt COMP-R2) (make-item "RaspberryPi" 2 101 #t
                                               (make-item "monitor" 1 135 #f EMPTY)))

(define (new-receipt ri)
  (if
   (string? ri) EMPTY
   (if (> (item-unit ri) 100)
       (make-item
        (item-desc ri)
        (item-qty ri)
        (item-unit ri)
        (item-sale? ri)
        (new-receipt
         (item-next ri)))
       (new-receipt (item-next ri)))))