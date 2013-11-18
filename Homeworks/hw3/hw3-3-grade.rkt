#lang racket

(require "hw3-3-library.rkt")
(require "hw3-3.rkt")
(require "common-grade.rkt")

(require racket/set)

(define (index-of l x)
  (if (equal? x 0) (car l)
      (index-of (cdr l) (- x 1))))

(define (move-location x y d)
  (cond [(equal? d 0) (cons x (- y 1))]
        [(equal? d 1)
         (if (odd? x)
             (cons (+ x 1) (- y 1))
             (cons (+ x 1) y))]
        [(equal? d 2)
         (if (odd? x)
             (cons (+ x 1) y)
             (cons (+ x 1) (+ y 1)))]
        [(equal? d 3) (cons x (+ y 1))]
        [(equal? d 4)
         (if (odd? x)
             (cons (- x 1) y)
             (cons (- x 1) (+ y 1)))]
        [(equal? d 5)
         (if (odd? x)
             (cons (- x 1) (- y 1))
             (cons (- x 1) y))]))

(define (range-to n)
  (let f ([i (- n 1)] [acc '()])
    (if (equal? i 0) (cons i acc)
        (f (- i 1) (cons i acc)))))

(define (check-maze maze)
  (define n (car (car maze)))
  (define m (cdr (car maze)))
  (define (valid-location x y)
    (and (<= 0 x) (< x n) (<= 0 y) (< y m)))
  (define lst (cdr maze))
  (define (lookup x y d)
    (let* ([col (index-of lst x)]
           [cell (index-of col y)])
      (index-of cell d)))
  (define tops (filter (lambda (x) (lookup x 0 0)) (range-to n)))
  (define bots (filter (lambda (x) (lookup x (- m 1) 3)) (range-to n)))

  (cond
   [(not (equal? (length tops) 1)) #f]
   [(not (equal? (length bots) 1)) #f]
   [else
    (begin
      (define top (car tops))
      (define bot (car bots))
      (define visit
        (let fixpt ([visit (set (cons top 0))]
                    [worklist (set (cons top 0))])
          (if
           (set-empty? worklist) visit
           (let*
               ([xy (set-first worklist)]
                [wl (set-rest worklist)]
                [x (car xy)]
                [y (cdr xy)]
                [vw
                 (foldl
                  (lambda (d vw)
                    (begin
                      (define v (car vw))
                      (define w (cdr vw))
                      (define nxy (move-location x y d))
                      (define nx (car nxy))
                      (define ny (cdr nxy))
                      (if (and
                           (valid-location nx ny)
                           (lookup x y d)
                           (not (set-member? v nxy)))
                          (cons (set-add v nxy) (set-add w nxy))
                          vw)))
                  (cons visit wl)
                  (range-to 6))])
             (fixpt (car vw) (cdr vw))))))
      (set-member? visit (cons bot (- m 1))))]))

(define maze1 (init-maze 3 3))
(define maze2 (open-n 1 0 maze1))
(define maze3 (open-n 1 1 maze2))
(define maze4 (open-n 1 2 maze3))
(define maze5 (open-s 1 2 maze4))
(define maze6 (open-nw 2 2 maze4))
(define maze7 (open-s 2 2 maze6))
(output (lambda () (equal? #t (check-maze maze1))))
(output (lambda () (equal? #t (check-maze maze2))))
(output (lambda () (equal? #t (check-maze maze3))))
(output (lambda () (equal? #t (check-maze maze4))))
(output (lambda () (equal? #t (check-maze maze5))))
(output (lambda () (equal? #t (check-maze maze6))))
(output (lambda () (equal? #t (check-maze maze7))))

;;

(output (lambda () (equal? #t (check-maze (mazeGen 3 3)))))
(output (lambda () (equal? #t (check-maze (mazeGen 5 5)))))
