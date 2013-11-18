#lang racket

(provide crazy2val)

(define (crazy2val crazy2)
  (if (equal? crazy2 'n) ; initial condition
      -1
      (if (equal? crazy2 'z) ; initial condition
          0
          (if (equal? crazy2 'p) ; initial condition
              1
              (if (equal? (car crazy2) 'z) ; if crazy2 is not single pair, then call car funtion
                  (* 2 (crazy2val (cdr crazy2))) ; this is property of crazy2 when add one component to left side
                     (if (equal? (car crazy2) 'p)
                         (+ (* 2 (crazy2val (cdr crazy2))) 1)
                         (if (equal? (car crazy2) 'n)
                             (- (* 2 (crazy2val (cdr crazy2))) 1)
                             null
                             )
                         )
                     )
              )
          )
      )
  )
    