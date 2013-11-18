#lang racket

;;; It is recommended to see hw5-2-grade.rkt before doing your HW.

; If these statements are omitted, your submission will be graded 0.
(provide catchYou)

; catchYou: graph * int -> (store X real) list
;
; This task is a challenge, so TAs will test with large input
; cases. Only reasonably efficient algorithm will pass test cases.
;
; Output should be ordered by the stores' name. Note that stores' name
; are one of: 'A, 'B, 'C, 'D, or 'E.
;
; Only stores appeared in the input are considered as the starting
; point, and those only should be output.
(define (row-matrix model store)
  (if (null? model)
      null
      (if (equal? (car (car model)) store)
          (cons (car model) (row-matrix (cdr model) store))
          (row-matrix (cdr model) store))))

(define (column-matrix model store)
  (if (null? model)
      null
      (if (equal? (car (cdr (car model))) store)
          (cons (car model) (column-matrix (cdr model) store))
          (column-matrix (cdr model) store))))

(define (row-matrix-to-list row-matrix)
  (let ([a 0] [b 0] [c 0] [d 0] [e 0])
    (define (to-list row)
      (cond ((equal? row null) (list a b c d e))
            ((equal? (list-ref (car row) 1) 'A) (set! a (list-ref (car row) 2))
                                                (to-list (cdr row)))
            ((equal? (list-ref (car row) 1) 'B) (set! b (list-ref (car row) 2))
                                                (to-list (cdr row)))
            ((equal? (list-ref (car row) 1) 'C) (set! c (list-ref (car row) 2))
                                                (to-list (cdr row)))
            ((equal? (list-ref (car row) 1) 'D) (set! d (list-ref (car row) 2))
                                                (to-list (cdr row)))
            ((equal? (list-ref (car row) 1) 'E) (set! e (list-ref (car row) 2))
                                                (to-list (cdr row)))))
    (to-list row-matrix)))

(define (column-matrix-to-list column-matrix)
  (let ([a 0] [b 0] [c 0] [d 0] [e 0])
    (define (to-list column)
      (cond ((equal? column null) (list a b c d e))
            ((equal? (list-ref (car column) 0) 'A) (set! a (list-ref (car column) 2))
                                                (to-list (cdr column)))
            ((equal? (list-ref (car column) 0) 'B) (set! b (list-ref (car column) 2))
                                                (to-list (cdr column)))
            ((equal? (list-ref (car column) 0) 'C) (set! c (list-ref (car column) 2))
                                                (to-list (cdr column)))
            ((equal? (list-ref (car column) 0) 'D) (set! d (list-ref (car column) 2))
                                                (to-list (cdr column)))
            ((equal? (list-ref (car column) 0) 'E) (set! e (list-ref (car column) 2))
                                                (to-list (cdr column)))))
    (to-list column-matrix)))

(define (multiply row column)
  (if (null? row)
      0
      (+ (* (car row) (car column)) (multiply (cdr row) (cdr column)))))

(define (make-row-matrix model)
  (let ([row1 (row-matrix-to-list (row-matrix model 'A))]
        [row2 (row-matrix-to-list (row-matrix model 'B))]
        [row3 (row-matrix-to-list (row-matrix model 'C))]
        [row4 (row-matrix-to-list (row-matrix model 'D))]
        [row5 (row-matrix-to-list (row-matrix model 'E))])
  (list row1 row2 row3 row4 row5)))

(define (make-column-matrix model)
  (let ([column1 (column-matrix-to-list (column-matrix model 'A))]
        [column2 (column-matrix-to-list (column-matrix model 'B))]
        [column3 (column-matrix-to-list (column-matrix model 'C))]
        [column4 (column-matrix-to-list (column-matrix model 'D))]
        [column5 (column-matrix-to-list (column-matrix model 'E))])
  (list column1 column2 column3 column4 column5)))

(define (multiply-to-column-matrix row-matrix column-matrix)
  (define (column-i row-matrx column-matrx i)
    (if (null? (cdr row-matrx))
        (list (multiply (car row-matrx) (list-ref column-matrx (+ -1 i))))
        (append (list (multiply (list-ref row-matrx 0) (list-ref column-matrx (+ -1 i)))) (column-i (cdr row-matrx) column-matrx i))))
  (let ([column1 (column-i row-matrix column-matrix 1)]
        [column2 (column-i row-matrix column-matrix 2)]
        [column3 (column-i row-matrix column-matrix 3)]
        [column4 (column-i row-matrix column-matrix 4)]
        [column5 (column-i row-matrix column-matrix 5)])
    (list column1 column2 column3 column4 column5)))

(define (multiply-with-row row matrix)
  (let ([column1 (multiply row (list-ref matrix 0))]
        [column2 (multiply row (list-ref matrix 1))]
        [column3 (multiply row (list-ref matrix 2))]
        [column4 (multiply row (list-ref matrix 3))]
        [column5 (multiply row (list-ref matrix 4))])
    (list column1 column2 column3 column4 column5)))

(define (catchYou model step)
  (let ([column-matrix (make-column-matrix model)]
        [row-matrix (make-row-matrix model)]
        [n step])
    (define (prob-matrix column-matrx i)
      (if (equal? i 1)
           column-matrx
          (multiply-to-column-matrix row-matrix (prob-matrix column-matrx (- i 1)))))
    (let ((lst (map (lambda (x) (* 100 x)) (multiply-with-row (list 0.25 0.25 0.25 0.25 0) (prob-matrix column-matrix n)))))
      (list (cons 'A (list-ref lst 0)) (cons 'B (list-ref lst 1)) (cons 'C (list-ref lst 2)) (cons 'D (list-ref lst 3))))))

    