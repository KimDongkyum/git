#lang racket

(provide zipperN)


(define (first-zipper lists) ; for the first zipping element
  (if (equal? lists null) ; if lists is null, then returns null
      null
      (if (equal? (car lists) null) ; if (car lists) is null, then returns (first-zipper (cdr lists))
          (first-zipper (cdr lists))
          (append (list (car (car lists))) (first-zipper (cdr lists)))  ; this is first zipped list, for example (first-zipper '((1 2 3) (a b c) (A B C))) returns '(1 a A)
      )
  )
)

(define (nextlists lists) ; for zipping next lists
  (if (equal? lists null)
      null
      (if (equal? (car lists) null)
          (nextlists (cdr lists))
          (append (list (cdr (car lists))) (nextlists (cdr lists))) ; this is the next lists to be zipped, for example (nextlists '((1 2 3) (a b c) (A B C))) returns '((2 3) (b c) (B C))
      )
  )
)
(define (zipperN lists)
  (if (equal? lists null)
      null
      (if (equal? (car lists) null)
          (zipperN (cdr lists))
          (if (equal? (cdr (car lists)) null) ; if (car lists) has single element, then returns '( (car lists) (zipperN (cdr lists)) )
              (append (list (car (car lists))) (zipperN (cdr lists)))
              (append (first-zipper lists) (zipperN (nextlists lists))) ; this is first step to zip. zipping first elements of lists, then append (zipperN (nextlists lists))
              )
          )
      )
  )