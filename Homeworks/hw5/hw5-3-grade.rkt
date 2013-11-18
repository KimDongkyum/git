#lang racket

(require "common-grade.rkt")
(require "hw5-3.rkt")


;;; Tape
(printf "Tape\n")

(define tape1 (init-tape (list "a" "b")))
(define tape2 (move-tape-left tape1))
(define tape3 (move-tape-left tape2))
(define tape4 (move-tape-right tape3))
(define tape5 (move-tape-right tape4))
(define tape6 (move-tape-right tape5))
(define tape7 (write-tape tape6 "c"))

(output (lambda () (equal? "-.-.a.b.-" (print-tape tape1 2))))
(output (lambda () (equal? "-.a.b.-.-" (print-tape tape2 2))))
(output (lambda () (equal? "a.b.-.-.-" (print-tape tape3 2))))
(output (lambda () (equal? "-.a.b.-.-" (print-tape tape4 2))))
(output (lambda () (equal? "-.-.a.b.-" (print-tape tape5 2))))
(output (lambda () (equal? "-.-.-.a.b" (print-tape tape6 2))))
(output (lambda () (equal? "-.-.c.a.b" (print-tape tape7 2))))

(output (lambda () (equal? "a" (read-tape tape1))))
(output (lambda () (equal? "b" (read-tape tape2))))
(output (lambda () (equal? "c" (read-tape tape7))))


;;; Rule table
(printf "Rule table\n")

(define table1 empty-ruletable)
(define table2 (add-rule (make-rule "1" "a" "d" 'right "2") table1))
(define table3 (add-rule (make-rule "2" "b" "e" 'right "3") table2))
(define table4 (add-rule (make-rule "3" "-" "f" 'left "4") table3))
(define table5 (add-rule (make-rule "4" "e" "g" 'left "5") table4))
(define table6 (add-rule (make-rule "5" "d" "h" 'left "6") table5))
(define table7 (add-rule (make-rule "6" "-" "i" 'stay "7") table6))

(output (lambda () (equal? (cons "d" (cons 'right "2")) (match-rule "1" "a" table7))))
(output (lambda () (equal? (cons "e" (cons 'right "3")) (match-rule "2" "b" table7))))
(output (lambda () (equal? (cons "f" (cons 'left "4")) (match-rule "3" "-" table7))))
(output (lambda () (equal? (cons "g" (cons 'left "5")) (match-rule "4" "e" table7))))
(output (lambda () (equal? (cons "h" (cons 'left "6")) (match-rule "5" "d" table7))))
(output (lambda () (equal? (cons "i" (cons 'stay "7")) (match-rule "6" "-" table7))))


;;; Turaing machine
(printf "Turing machine\n")

(define tm1 (make-tm (list "a" "b") "1" table7))
(define tm2 (step-tm tm1))
(define tm3 (step-tm tm2))
(define tm4 (step-tm tm3))
(define tm5 (step-tm tm4))
(define tm6 (step-tm tm5))
(define tm7 (step-tm tm6))
(define tm8 (step-tm tm7))
(define tm9 (run-tm tm1))
(define tm10 (run-tm tm8))

(output (lambda () (equal? "-.-.a.b.-" (print-tm tm1 2))))
(output (lambda () (equal? "-.d.b.-.-" (print-tm tm2 2))))
(output (lambda () (equal? "d.e.-.-.-" (print-tm tm3 2))))
(output (lambda () (equal? "-.d.e.f.-" (print-tm tm4 2))))
(output (lambda () (equal? "-.-.d.g.f" (print-tm tm5 2))))
(output (lambda () (equal? "-.-.-.h.g" (print-tm tm6 2))))
(output (lambda () (equal? "-.-.i.h.g" (print-tm tm7 2))))

; If no rule is applicable, just do nothing.
(output (lambda () (equal? "-.-.i.h.g" (print-tm tm8 2))))

(output (lambda () (equal? "-.-.i.h.g" (print-tm tm9 2))))
(output (lambda () (equal? "-.-.i.h.g" (print-tm tm10 2))))
