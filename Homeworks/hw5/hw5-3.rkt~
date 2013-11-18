#lang racket

;;; It is recommended to see hw5-2-grade.rkt before doing your HW.

; If these statements are omitted, your submission will be graded 0.
(provide init-tape read-tape write-tape move-tape-left move-tape-right print-tape)
(provide empty-ruletable add-rule make-rule match-rule)
(provide make-tm step-tm run-tm print-tm)


;;; Tapes

(define (init-tape syms) ; init-tape: symbol list -> tape
  'TODO)

(define (read-tape tape) ; read-tape: tape -> symbol
  'TODO)

(define (write-tape tape sym) ; write-tape: tape * symbol -> tape
  'TODO)

(define (move-tape-left tape) ; move-tape-left: tape -> tape
  'TODO)

(define (move-tape-right tape) ; move-tape-right: tape -> tape
  'TODO)

; Implement "tape * int -> string" instead of "tap -> void".
; The int argument is the size of printed tapes as in print-tm.
(define (print-tape tape size) ; print-tape: tape * int -> string
  'TODO)


;;; Rule tables

(define empty-ruletable ; empty-ruletable: ruletable
  'TODO)

(define (add-rule rule table) ; add-rule: rule * ruletable -> ruletable
  'TODO)

(define (make-rule curstate cursym newsym move newstate) ; make-rule: state * symbol * symbol * move * state -> rule
  'TODO)

(define (match-rule curstate cursym table) ; match-rule: state * symbol * ruletable -> symbol X move X state
  'TODO)


;;; Turing machines

(define (make-tm syms state ruletable) ; make-tm: symbol list * state * ruletable -> tm
  'TODO)

(define (step-tm tm) ; step-tm: tm -> tm
  'TODO)

(define (run-tm tm) ; run-tm: tm -> tm
  'TODO)

; Implement "tm * int -> string" instead of "tm * int -> void".
(define (print-tm tm size) ; print-tm: tm * int -> string
  'TODO)
