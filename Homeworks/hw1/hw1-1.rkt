#lang racket

(provide gcd)

(define (gcd n m) ; define gcd function
  (if (or (< n 0) (< m 0))
      -1 ; if n or m is negative integer, then returns -1 means error
      (if (or (= n 0) (= m 0))
          (if (<= n m) m n) ; if n or m is zero, then returns bigger integer
          (if (< n m) (gcd n (- m n)) (gcd (- n m) m)) ; if n and m are non-zero, then returns |m-n|. This is property of gcd fucntion
          )
      )
  )
