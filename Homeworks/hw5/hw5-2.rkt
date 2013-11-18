#lang racket


;;; It is recommended to see hw5-2-grade.rkt before doing your HW.

;;; If these statements are omitted, your submission will be graded 0.

(provide equal)
(provide size)
(provide beautiful)



; You can use the definitions and functions defined in hw5-1.rkt:
; black, white, glue-array-from-tree, glue-array-from-array,
; rotate-array, neighbor-array, pprint-array, is-array?,
; glue-tree-from-tree, glue-tree-from-array, rotate-tree,
; neighbor-tree, pprint-tree, is-tree?, array-to-tree, tree-to-array,
; glue, rotate, neighbor, pprint
(require "hw5-1.rkt")


;;; interfaces
(define (equal f g) ; equal: form * form -> bool
  (cond ((and (is-array? f) (is-array? g)) (equal? f g))
        ((and (is-array? f) (is-tree? g)) (equal? (array-to-tree f) g))
        ((and (is-tree? f) (is-array? g)) (equal? f (array-to-tree g)))
        ((and (is-tree? f) (is-tree? g)) (equal? f g))))

(define (tree-size tree)
    (if (not (list? tree))
        0
        (+ 1 (tree-size (car tree)))))

(define (array-size array)
  (if (equal? 1 (length array))
      0
      (+ 1 (array-size (list-tail array (/ (length array) 2))))))
      

(define (size f) ; size: form -> int
  (cond ((not (pair? f)) 0)
        ((is-tree? f) (tree-size (cdr f)))
        ((is-array? f) (array-size (cdr f)))))

;사진법 형태의 리스트로 바꾸는 함수
(define (loc i size)
  (if (equal? size 1)
      (list (remainder i 4))
      (append (loc (quotient i 4) (+ -1 size)) (list (remainder i 4)))))

(define (beautiful f) ; beautiful: form -> bool
  (let ((n 0))
  (define (beautiful-sym f)
    (equal? f (rotate (rotate f))))
  (define (beautiful-neighbor f)
    (for ([i (expt 4 (size f))])
          #:break (or (< (neighbor (loc i (size f)) f) 2) (> (neighbor (loc i (size f)) f) 5))
          (set! n i)
      )
    (and (equal? n (+ -1 (expt 4 (size f))))
         (not (or (< (neighbor (loc n (size f)) f) 2) (> (neighbor (loc n (size f)) f) 5)))))
  (or (beautiful-sym f) (beautiful-neighbor f))))