#lang racket

(provide t2)


(define (t1 n) ; n-th element of (t2 n)
  (if (< n 0) ; for negative n integer
      (t1 (* n -1)) ; make n absolute value
      (if (= n 0)
          null
          (if (= n 1) "1" (string-append "1" (t1 (- n 1)))) ; recursion function
          )
      )
  )

(define (t2 n)
  (if (< n 0) ; for negative n integer
      (t2 (* n -1)) ; make n absolute value
      (if (= n 0)
          "0"
          (string-append (t2 (- n 1)) (string-append "0" (t1 n)) ) ; using t1 fucntion and recursion function
          )
      )
  )