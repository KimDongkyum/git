#lang racket

(require "hw3-2-library.rkt")

(provide maze-check)

(define (n-element n l) ; n-element : int*(room list) -> room
  (if (null? (cdr l))
      (car l)
      (if (= n 1)
          (car l)
          (n-element (- n 1) (cdr l) )
          )
      )
  )

(define (add-list l s) ; add-list : (room list)*(room set) -> (room set)
  (define (loop m)
    (if (= m 1)
        (if (is-member? (n-element 1 l) s)
            s
            (add-element (n-element 1 l) s)
            )
        (if (is-member? (n-element m l) s)
            (loop (- m 1))
            (add-element (n-element m l) (loop (- m 1)))
            )
        )
    )
  (loop (length l))
  )

(define (can-enter-set room maze) ; can-enter-set : room*maze -> room set
  (let (first-can-enter (add-list (can-enter room maze) (add-element room empty-set))))
  (define (can-enter-prime roomlist set1 set2 m n maze)
    (cond (= m n) set1)
    (cond (< m n) (can-enter-pirme roomlist set2 (append set1 set2) (length set2) (length (append set1 set2)) maze))
    (else null)
          
    
        (add-list (can-enter (n-element 2 (loop (- m 1))) maze) (add-list (can-enter (n-element 1 (loop (- m 1))) maze) (loop (- m 1))))
            )
    )
  (loop 9)
  )

(define (maze-check maze start end)
  (is-member? end (can-enter-set start maze))
  )
