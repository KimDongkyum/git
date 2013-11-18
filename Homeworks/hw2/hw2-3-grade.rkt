#lang racket

(require "common-grade.rkt")
(require "hw2-3.rkt")

(define (iterGrade)
  (begin
    (printf "iter\n")
		(output (lambda () (equal? ((iter 5 (lambda (x) (+ 2 x))) 0) 10)))
		(output (lambda () (equal? ((iter 5 (lambda (x) (+ 3 x))) 1) 16)))))

(iterGrade)
