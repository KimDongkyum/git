#lang racket

(provide output)

(define (output p)
  (printf (if (equal? p #t) "O\n" "X\n")))
