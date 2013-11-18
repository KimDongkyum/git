#lang racket

(require "common-grade.rkt")
(require "hw2-1.rkt")

(define (zipperGrade)
  (begin
    (printf "zipper\n")
		(output (lambda () (equal? (zipper '(1) '(2)) '(1 2))))
		(output (lambda () (equal? (zipper '(1 3 5) '(2 4)) '(1 2 3 4 5))))))

(zipperGrade)
