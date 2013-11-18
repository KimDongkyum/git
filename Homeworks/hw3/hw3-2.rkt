#lang racket

(require "hw3-2-library.rkt")

(provide maze-check)

;fucntion (n-element n l) returns the n-th element of the list l

(define (n-element n l)
  (if (null? (cdr l))
      (car l)
      (if (= n 1)
          (car l)
          (n-element (- n 1) (cdr l))))
  )

;(add-list l s)함수는 set s에다가 list l의 원소들 모두를 각각 set에 포함되있는지 확인하여 추가시키는 함수다.

(define (add-list l s)
  (define (loop m)
    (if (= m 1)
        (if (is-member? (n-element 1 l) s)
            s
            (add-element (n-element 1 l) s))
        (if (is-member? (n-element m l) s)
            (loop (- m 1))
            (add-element (n-element m l) (loop (- m 1)))))
    )
  (loop (length l))
  )

;(enter-list-set l maze)함수는 room list 인 l의 모든 원소가 각각 들어갈수 있는 (can-enter room)의 set이다.

(define (enter-list-set l maze)
  (if (null? (cdr l))
      (add-list (can-enter (car l) maze) l)
      (add-list (can-enter (car l) maze) (enter-list-set (cdr l) maze))))

;(can-enter-set room maze)함수는 room에서 이어져 있는 모든 room들의 set이다
;(can-enter-set set1 set2 m n maze)함수는 끝재귀함수로써 m과 n은 set1과 set2의 원소의 갯수이고,
;만일 m과 n이 같다면 set1을 리턴한다.(set1과 set2가 같다는 의미이기 때문에)

(define (can-enter-set room maze)
  (define (can-enter-prime set1 set2 m n maze)
    (cond ((= m n) set1)
          ((< m n) (can-enter-prime set2 (enter-list-set set2 maze) (length set2) (length (enter-list-set set2 maze)) maze))
          (else null))
    )
    (can-enter-prime '() (list room) 0 1 maze)
  )

;end room 이 (can-enter-set start maze)의 원소라면 미로의 조건을 만족한다.

(define (maze-check maze start end)
  (is-member? end (can-enter-set start maze))
  )