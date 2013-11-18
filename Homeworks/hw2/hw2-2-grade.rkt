#lang racket

(require "common-grade.rkt")
(require "hw2-2.rkt")

(define (zipperNGrade)
  (begin
    (printf "zipperN\n")
		(output (lambda () (equal? (zipperN '((1) (2) (3))) '(1 2 3))))
		(output (lambda () (equal? (zipperN '((1) () (3))) '(1 3))))
		(output (lambda () (equal? (zipperN '((1) (2 4 5 6) (3))) '(1 2 3 4 5 6))))))

(zipperNGrade)
