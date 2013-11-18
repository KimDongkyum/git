#lang racket

(provide output)

(define (output p)
  (with-handlers
	 ([(lambda (exn) #t) (lambda (exn) (printf "X (error)\n"))])
	 (printf (if (equal? (p) #t) "O\n" "X\n"))))
