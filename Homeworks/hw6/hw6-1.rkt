#lang racket
(require r5rs)

;;; If these statements are omitted, your submission will be graded 0.
(provide memo-ways)

;; Look at the textbook 3.3.3 Two-dimensional tables

(define (make-table)
  (list 'table))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
         (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1 (cons key-2 value)) (cdr table)))))
'ok)

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x y)
      (let ((previously-computed-result (lookup x y table)))
        (if previously-computed-result
            previously-computed-result
            (let ((result (f x y)))
              (insert! x y result table)
              result))))))

(define memo-ways  ; memo-ways: int * int -> int
  (memoize (lambda (n m)
             (cond ((= n 0) 1)
                   ((= m 0) 1)
                   (else (+ (memo-ways (- n 1) m)
                            (memo-ways n (- m 1))))))))