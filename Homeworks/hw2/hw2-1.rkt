#lang racket

(provide zipper)

(define (zipper lhs rhs)
  (if (equal? lhs '())
      rhs
  (if (equal? rhs '()) ; if rhs is null, then return lhs
      lhs
      (if (equal? (cdr rhs) '()) ; if rhs has single element, then return '( (car lhs) rhs (cdr lhs) )
          (append (list (car lhs) (car rhs)) (cdr lhs))
          (append (list (car lhs) (car rhs)) (zipper (cdr lhs) (cdr rhs))) ; if rhs has not single emlement, then return '( (car lhs) (car rhs) (zipper (cdr lhs) (cdr rhs)) )
          )
      )
  )
  )