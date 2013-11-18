#lang racket

(provide crazy2add)

; nadd, zadd and padd function are frequently called in crazy2add. So defining those functions outside of crazy2add fucntion.

(define (nadd rhs) ; if lhs is 'n, then called this function
  (if (equal? rhs 'n) ; initial condition 'n + 'n = '( z . n)
          (cons 'z 'n)
          (if (equal? rhs 'z) ; initial condition 'n + 'z = '( n . z)
              (cons 'n 'z) ; this is equal to 'n , but this form is more convinient to use as parameter of fucntion crazy2add
              (if (equal? rhs 'p)
                  (cons 'z 'z) ; same reason 'n + 'p = '( z . z)
                  (if (equal? (car rhs) 'n)
                      (cons 'z (nadd (cdr rhs))) ; called nadd function recusively, carry is 'n
                      (if (equal? (car rhs) 'z)
                          (cons 'n (cdr rhs)) ; carry is 'z
                          (if (equal? (car rhs) 'p)
                              (cons 'z (cdr rhs)) ; carry is 'z
                              null
                              )
                          )
                      )
                  )
              )
          )
  )
  

(define (zadd rhs) ; if lhs is 'z, then called this function
  (if (equal? rhs 'n)
      (cons rhs 'z) ; for convinient 'z + 'n = '( n . z)
      (if (equal? rhs 'z)
          (cons rhs 'z) ; 'z + 'z = '( z . z)
          (if (equal? rhs 'p)
              (cons rhs 'z) ; 'z + 'p = '( p . z)
              rhs
              )
          )
      )
  )

(define (padd rhs) ; if lhs is 'p, then called this function
  (if (equal? rhs 'n)
          (cons 'z 'z) ; 'p + 'n = '( z . z)
          (if (equal? rhs 'z)
              (cons 'p 'z) ; 'p + 'z = '( p . z)
              (if (equal? rhs 'p)
                  (cons 'z 'p) ; 'p + 'p = '( z . p)
                  (if (equal? (car rhs) 'n)
                      (cons 'z (cdr rhs)) ; carry is 'z
                      (if (equal? (car rhs) 'z)
                          (cons 'p (cdr rhs)) ; carry is 'z
                          (if (equal? (car rhs) 'p)
                              (cons 'z (padd (cdr rhs))) ; called padd function recursively, carry is 'p
                              null
                              )
                          )
                      )
                  )
              )
          )
  )
  

(define (crazy2add lhs rhs)
  (if (equal? lhs 'n)
     (nadd rhs)
     (if (equal? lhs 'z)
         (zadd rhs)
         (if (equal? lhs 'p)
             (padd rhs)
             (if (equal? (car lhs) 'n)
                 (cons (car (nadd rhs)) (crazy2add (cdr lhs) (cdr (nadd rhs)))) ; called crazy2add recursively, and called nadd function
                 (if (equal? (car lhs) 'z)
                     (cons (car (zadd rhs)) (crazy2add (cdr lhs) (cdr (zadd rhs)))) ; called crazy2add recursively, and called zadd function
                     (if (equal? (car lhs) 'p)
                         (cons (car (padd rhs)) (crazy2add (cdr lhs) (cdr (padd rhs)))) ; called crazy2add recursively, and called padd fucntion
                         null
                         )
                     )
                 )
             )
         )
     )
  )