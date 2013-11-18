#lang racket

(require "common-grade.rkt")
(require "hw5-4.rkt")

(define (item-match? l r)
  (and (equal? (car l) (car r))
       (let ([ratio (/ (cdr l) (cdr r))])
         (and (< 0.99 ratio) (< ratio 1.01)))))

(define (list-match? pred l r)
  (cond [(equal? l '()) (equal? r '())]
        [(equal? r '()) #f]
        [else (and (pred (car l) (car r)) (list-match? pred (cdr l) (cdr r)))]))

(output
 (lambda ()
   (let* ([input '((A B 1.0) (B C 0.3) (B D 0.7) (C A 0.6) (C D 0.4) (D D 1.0))]
          [model-output (list (cons 'A 15.0) (cons 'B 25.0) (cons 'C 7.5) (cons 'D 52.5))]
          [output (catchYou input 1)])
     (printf "~s~n" model-output)
     (printf "~s~n" output)
     (list-match? item-match? model-output output))))

(output
 (lambda ()
   (let* ([input '((A B 1.0) (B C 0.3) (B D 0.7) (C A 0.6) (C D 0.4) (D D 1.0))]
          [model-output (list (cons 'A 4.5) (cons 'B 15.0) (cons 'C 7.5) (cons 'D 73.0))]
          [output (catchYou input 2)])
     (printf "~s~n" model-output)
     (printf "~s~n" output)
     (list-match? item-match? model-output output))))

(output
 (lambda ()
   (let* ([input '((A B 1.0) (B C 0.3) (B D 0.7) (C A 0.6) (C D 0.4) (D C 0.3) (D D 0.7))]
          [model-output '((A . 12.162162161758499) (B . 12.162162161758502) (C . 20.2702702695975) (D . 55.4054054035665))]
          [output (catchYou input 1000000)])
     (printf "~s~n" model-output)
     (printf "~s~n" output)
     (list-match? item-match? model-output output))))
