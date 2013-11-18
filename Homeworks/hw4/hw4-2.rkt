#lang racket


; We auto-grade only react function; other functions are not
; auto-graded. However, S, K, I, v, and a are required for
; grading. See hw4-2-grade.rkt for more information.
; If this statement is omitted, your submission will be graded 0.
(provide react S K I v a)


; Implement react. The contents provided below can be modified,
; unless the modification does not change the type and satisfies the spec.
;
; In the document, react has the type solution -> void.
; However, implement react: solution -> string for ease of grading.
; Return the string of the given solution printed by pprint.
;
; See hw4-2-grade.rkt for more information on what the returned string should look like.
; In short,
; S prints "S";
; K prints "K";
; I prints "I";
; variable x prints "x";
; tuple (E F) prints "(" + E + " " + F + ")".
(define (react solution) ; react: solution -> string
  (pprint solution))

(define S ; S: solution
  (cons 'S (lambda (x) (lambda (y) (lambda (z) ((x z) (y z)))))))
(define K ; K: solution
  (cons 'K (lambda (x) (lambda (y) x))))
(define I ; I: solution
  (cons 'I (lambda (x) x)))
(define (v str) ; v: string -> solution
  (letrec ((f (lambda (x) (if (number? x)
                           str
                           (if (string? (x 1))
                               (cons (v str) (v (x 1)))
                               f
                               )
                           )
                )))
    (cons 'v f)
    )
  )
(define (a lhs rhs) ; v: solution * solution -> solution
  (if (pair? (cdr lhs))
      (cons 'a (cons lhs rhs))
      (cons 'a ((cdr lhs) (cdr rhs)))
      )
  )
  
; You may need the following tree interface.

(define (isS? solution); isS?: solution -> bool
  (equal? (car solution) 'S))
(define (isK? solution) ; isK?: solution -> bool
  (equal? (car solution) 'K))
(define (isI? solution) ; isI?: solution -> bool
  (equal? (car solution) 'I))
(define (isv? solution) ; isv?: solution -> bool
  (equal? (car solution) 'v))
(define (isa? solution) ; isa?: solution -> bool
  (equal? (car solution) 'a))
(define (var solution) ; var: solution -> string
  ((cdr solution) 1))
(define (al solution) ; al: solution -> solution
  (if (pair? (cdr solution))
      (car (cdr solution))
      (cdr solution)
      )
  )
(define (ar solution) ; ar: solution -> solution
  (if (pair? (cdr solution))
      (cdr (cdr solution))
      null
      )
  )
(define (pprint solution) ; pprint: solution -> string
 (cond ((isS? solution) "S")
       ((isK? solution) "K")
       ((isI? solution) "I")
       ((isv? solution) (var solution))
       ((isa? solution) (if (null? (ar solution))
                            ((al solution) 1)
                            (string-append "(" (pprint (al solution)) " " (pprint (ar solution)) ")")
                            )
                        )
       )
  )
; Write down types for each declaration and functions.
; -1 points for each wrong type annotation (-5 points top).
