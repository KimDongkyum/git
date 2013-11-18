#lang racket

;;; It is recommended to see hw5-2-grade.rkt before doing your HW.

; If these statements are omitted, your submission will be graded 0.
(provide init-tape read-tape write-tape move-tape-left move-tape-right print-tape)
(provide empty-ruletable add-rule make-rule match-rule)
(provide make-tm step-tm run-tm print-tm)


;;; Tapes

(define (init-tape syms) ; init-tape: symbol list -> tape
  (cons (cons 'current (car syms)) (cdr syms)))

(define (read-tape tape) ; read-tape: tape -> symbol
  (if (pair? (car tape))
      (cdr (car tape))
      (read-tape (cdr tape))))

(define (write-tape tape sym) ; write-tape: tape * symbol -> tape
  (if (pair? (car tape))
      (cons (cons 'current sym) (cdr tape))
      (cons (car tape) (write-tape (cdr tape) sym))))

(define (move-tape-left tape) ; move-tape-left: tape -> tape
  (if (pair? (car tape))
      (if (null? (cdr tape))
          (cons (cdr (car tape)) (list (cons 'current "-")))
          (cons (cdr (car tape)) (cons (cons 'current (car (cdr tape))) (cdr (cdr tape)))))
      (cons (car tape) (move-tape-left (cdr tape)))))

(define (move-tape-right tape) ; move-tape-right: tape -> tape
  (if (pair? (car tape))
      (if (null? (cdr tape))
          (cons (cons 'current "-") (list (cdr (car tape))))
          (append (list (cons 'current "-")) (append (list (cdr (car tape))) (cdr tape))))
      (if (pair? (car (cdr tape)))
          (append (list (cons 'current (car tape))) (cdr (move-tape-right (cdr tape))))
          (cons (car tape) (move-tape-right (cdr tape))))
      )
  )

; Implement "tape * int -> string" instead of "tap -> void".
; The int argument is the size of printed tapes as in print-tm.

;define move-tape-right-n, move-tape-left-n for making print-tape
(define (move-tape-right-n tape n)
  (if (equal? n 1)
      (move-tape-right tape)
      (move-tape-right-n (move-tape-right tape) (- n 1)))
  )

(define (move-tape-left-n tape n)
  (if (equal? n 1)
      (move-tape-left tape)
      (move-tape-left-n (move-tape-left tape) (- n 1)))
  )

(define (print-tape tape size) ; print-tape: tape * int -> string
  (if (equal? size 0)
      (read-tape tape)
      (string-append (read-tape (move-tape-right-n tape size)) "." (print-tape tape (- size 1)) "." (read-tape (move-tape-left-n tape size)))
      )
  )

;;; Rule tables

(define empty-ruletable ; empty-ruletable: ruletable
  (lambda (x) (cons "empty" (cons 'stay 'end))))

;I make rule as (curstate X cursym) X ( newsym X move X newstate )
;And table is a function.
(define (add-rule rule table) ; add-rule: rule * ruletable -> ruletable
  (lambda (x)
    (if (equal? x (car rule))
        (cdr rule)
        (table x)))
  )

(define (make-rule curstate cursym newsym move newstate) ; make-rule: state * symbol * symbol * move * state -> rule
  (cons (cons curstate cursym) (cons newsym (cons move newstate))))

(define (match-rule curstate cursym table) ; match-rule: state * symbol * ruletable -> symbol X move X state
  (table (cons curstate cursym)))


;;; Turing machines

; I make Turing machines as tape X state X ruletable.
(define (make-tm syms state ruletable) ; make-tm: symbol list * state * ruletable -> tm
 (cons (init-tape syms) (cons state ruletable)))

(define (step-tm tm) ; step-tm: tm -> tm
  (let ([move (car (cdr (match-rule (car (cdr tm)) (read-tape (car tm)) (cdr (cdr tm)))))]
        [state (cdr (cdr (match-rule (car (cdr tm)) (read-tape (car tm)) (cdr (cdr tm)))))]
        [symbol (car (match-rule (car (cdr tm)) (read-tape (car tm)) (cdr (cdr tm))))])
    (cond ((equal? state 'end) (cons (car tm) (cons state (cdr (cdr tm)))))  ; do nothig when turing machine's state is at end
          ((equal? move 'stay) (cons (write-tape (car tm) symbol) (cons state (cdr (cdr tm)))))
          ((equal? move 'left) (cons (move-tape-right (write-tape (car tm) symbol)) (cons state (cdr (cdr tm)))))
          ((equal? move 'right) (cons (move-tape-left (write-tape (car tm) symbol)) (cons state (cdr (cdr tm))))))
    )
  )

(define (run-tm tm) ; run-tm: tm -> tm
  (if (equal? (car (cdr tm)) 'end)
      tm
      (run-tm (step-tm tm))))

; Implement "tm * int -> string" instead of "tm * int -> void".
(define (print-tm tm size) ; print-tm: tm * int -> string
  (print-tape (car tm) size))
