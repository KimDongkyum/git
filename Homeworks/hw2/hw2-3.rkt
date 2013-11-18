#lang racket

(provide iter)

(define (abs n) ; for the abosolute of n
  (if (< n 0)
      (* n -1)
      n
      )
  )

(define (iter n f)
  (lambda (x) ; the parameter x is integer
    (define (loop m) ; define function loop
      (if (= m 0) ; identity function
          x
          (if (= m 1) ; if m is 1, then return f(x)
              (f x)
              (f (loop (- m 1))) ; if m is not 1, then return f(loop (m-1))
              )
          )
      )
    (loop (abs n)) ; for the negative integer of n
    )
  )