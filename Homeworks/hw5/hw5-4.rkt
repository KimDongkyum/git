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

;제 노트북에서는 시행시간이 약 5초정도 나옵니다. 제 방법의 문제점은 굳이 계산할 필요가 없는 항들을 다 포함시켰다는 것 인데요.
;제 머리로는 도저히 이 굳이 계산할 필요없는 항들을 계산하고 넘기는 법을 생각해낼 수 가 없었습니다. 
;이 방법에 대해 짧게 아이디어를 써주시면 1점, 구체적인 코드라인을 통해서 설명해주시면 2점 드리겠습니다.

;기본 원리는 그래프의 각 노드에 대하여  다른 노드로 진행할 가능성을 행렬로 표현하여
;초기 상태 row vector에다 계속하여 행렬곱을 시행하는 것이다.
(define (is-exist? model store)
  (if (null? model)
      #f
      (or (or (equal? (list-ref (car model) 0) store) (equal? (list-ref (car model) 1) store)) (is-exist? (cdr model) store))))

(define (exist-store-list model)
  (define (return-store model store)
    (if (is-exist? model store)
        (list store)
        null))
  (let ([a (return-store model 'A)]
        [b (return-store model 'B)]
        [c (return-store model 'C)]
        [d (return-store model 'D)]
        [e (return-store model 'E)])
    (append a b c d e)))

(define (column-matrix model store)
  (if (null? model)
      null
      (if (equal? (car (cdr (car model))) store)
              (cons (car model) (column-matrix (cdr model) store))
              (column-matrix (cdr model) store))))

(define (all-zero? lst)
  (if (null? (cdr lst))
      (if (= 0 (car lst))
          #t
          #f)
      (if (= 0 (car lst))
          (all-zero? (cdr lst))
          #f)))
      
(define (column-matrix-to-list column-matrix model)
  (let ([a null] [b null] [c null] [d null] [e null])
    (if (is-exist? model 'A) (set! a (list 0)) (set! a null))
    (if (is-exist? model 'B) (set! b (list 0)) (set! b null))
    (if (is-exist? model 'C) (set! c (list 0)) (set! c null))
    (if (is-exist? model 'D) (set! d (list 0)) (set! d null))
    (if (is-exist? model 'E) (set! e (list 0)) (set! e null))
    (define (to-list column)
      (cond ((equal? column null) (append a b c d e))
            ((equal? (list-ref (car column) 0) 'A) (set! a (list (list-ref (car column) 2)))
                                                (to-list (cdr column)))
            ((equal? (list-ref (car column) 0) 'B) (set! b (list (list-ref (car column) 2)))
                                                (to-list (cdr column)))
            ((equal? (list-ref (car column) 0) 'C) (set! c (list (list-ref (car column) 2)))
                                                (to-list (cdr column)))
            ((equal? (list-ref (car column) 0) 'D) (set! d (list (list-ref (car column) 2)))
                                                (to-list (cdr column)))
            ((equal? (list-ref (car column) 0) 'E) (set! e (list (list-ref (car column) 2)))
                                                (to-list (cdr column)))))
     (if (all-zero? (to-list column-matrix))
         null
         (list (to-list column-matrix)))))

(define (multiply row column)
  (if (null? column)
      null
      (if (null? (cdr row))
          (list (* (car row) (car column)))
          (list (+ (* (car row) (car column)) (car (multiply (cdr row) (cdr column))))))))

(define (make-column-matrix model)
  (let ([column1 (column-matrix-to-list (column-matrix model 'A) model)]
        [column2 (column-matrix-to-list (column-matrix model 'B) model)]
        [column3 (column-matrix-to-list (column-matrix model 'C) model)]
        [column4 (column-matrix-to-list (column-matrix model 'D) model)]
        [column5 (column-matrix-to-list (column-matrix model 'E) model)])
  (append column1 column2 column3 column4 column5)))

(define (my-list-ref lst i)
  (if (null? lst)
      null
      (if (null? (cdr lst))
          (if (= i 0)
              (car lst)
              null)
          (if (= i 0)
              (car lst)
              (my-list-ref (cdr lst) (- i 1))))))

(define (multiply-with-row row matrix)
  (let ([column1 (multiply row (my-list-ref matrix 0))]
        [column2 (multiply row (my-list-ref matrix 1))]
        [column3 (multiply row (my-list-ref matrix 2))]
        [column4 (multiply row (my-list-ref matrix 3))]
        [column5 (multiply row (my-list-ref matrix 4))])
    (append column1 column2 column3 column4 column5)))


(define (catchYou model step)
  (let ([column-matrix (make-column-matrix model)]
        [n step]
        [store-list (exist-store-list model)]
        [size (length (exist-store-list model))])
    (define (prob-matrix row i)
      (if (equal? i 1)
          (multiply-with-row row column-matrix)
          (multiply-with-row (prob-matrix row (- i 1)) column-matrix)))
    (define (init-row lst)
        (if (null? lst)
            null
            (append (list (/ 100 size)) (init-row (cdr lst)))))
    (let ([newlst (prob-matrix (init-row store-list) n)])
      (define (row-result fst lst)
        (if (null? fst)
            null
            (append (list (cons (car fst) (car lst))) (row-result (cdr fst) (cdr lst)))))
      (row-result store-list newlst))))