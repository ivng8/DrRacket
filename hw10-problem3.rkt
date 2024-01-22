;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/2: Design the data SimpleExpression that allows you to represent
;           arithmetic expressions given the following requirements:
;           - There are two valid operations: add (+) and multiply (*)
;           - Each operation is applied to at least one operand, which may be a
;             number or another expression
;
;           Some example expressions include...
;            5
;            3.14 * 2
;            (3 + 4) * (1 + -2 + 7) * (5)
;            
;           Make sure to follow all steps of the design recipe for data and
;           define at least the examples above.

(define-struct expression [oper rest])

; A SimpleExpression is one of:
; - number
; - (make-expression Operator [List-of SimpleExpression])
; Interpretation : a simple expression that can represent
; a number or a serie(s) of additions and multiplications

(define (simpexpress-temp se)
  (...
   (cond
     [(number? se) ...]
     [(expression? se) ...])))

; An Operator is a String that is one of:
; - "+"
; - "*"
; Interpretation : a string representation of an operator

(define ADD "+")
(define MULT "*")

(define (operator-temp op)
  (...
   (cond
     [(string=? op "+") ...]
     [(string=? op "*") ...])))

; TODO 2/2: Now design the function evaluate that takes a SimpleExpression and
;           produces its numerical result. So for the examples above (where <=
;           means that the expression on the right evaluates to the value on the
;           left...
;
;            5    <= 5
;            6.28 <= 3.14 * 2
;            210  <= (3 + 4) * (1 + -2 + 7) * (5)

(define 2pi (make-expression MULT (list 3.14 2)))
(define 3-branch (make-expression MULT (list
                                        (make-expression ADD (list 3 4))
                                        (make-expression ADD (list 1 -2 7))
                                        5)))

; evaluate : SimpleExpression -> Number
; evalutes your simple expression

(check-expect (evaluate 5) 5)
(check-expect (evaluate 2pi) 6.28)
(check-expect (evaluate 3-branch) 210)

(define (evaluate se)
  (local [(define (oper-iden op)
            (cond
              [(string=? ADD op) +]
              [(string=? MULT op) *]))]
    (cond
      [(number? se) se]
      [(expression? se)
       (foldr (oper-iden (expression-oper se))
              (oper-base (expression-oper se))
              (map evaluate (expression-rest se)))])))

; oper-base : Operator -> Number
; provides the base case for the evaluate function
; depending on the operator

(check-expect (oper-base ADD) 0)
(check-expect (oper-base MULT) 1)

(define (oper-base op)
  (cond
    [(string=? ADD op) 0]
    [(string=? MULT op) 1]))