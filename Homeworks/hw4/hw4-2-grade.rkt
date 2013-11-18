#lang racket

(require racket/match)
(require "common-grade.rkt")
(require "hw4-2.rkt")

(output
 (lambda ()
   (equal?
    (react (a (a (a S K) I) (v "x")))
    "x")))
(output
 (lambda ()
   (equal?
    (react (a (a K (v "x")) (a I (v "x"))))
    "x")))
(output
 (lambda ()
   (equal?
    (react (a (a (a (v "x") (v "y")) (v "z")) (v "w")))
    "(((x y) z) w)")))

(define (swap x y)
  (a (a (a (a S (a K (a S I))) K) x) y))

(output
 (lambda ()
   (let ([input (swap (v "x") (v "y"))])
     (equal?
      (react input)
      "(y x)"))))

(output
 (lambda ()
   (let ([input (swap (swap (v "x") (v "y")) (v "z"))])
     (equal?
      (react input)
      "(z (y x))"))))

(output
 (lambda ()
   (let ([input (swap (v "x") (swap (v "y") (v "z")))])
     (equal?
      (react input)
      "((z y) x)"))))

(output
 (lambda ()
   (let ([input (swap (swap (v "x") (v "y")) (swap (v "z") (v "w")))])
     (equal?
      (react input)
      "((w z) (y x))"))))

(output
 (lambda ()
   (let ([input (swap (swap (swap (v "x") (v "y")) (v "z")) (v "w"))])
     (equal?
      (react input)
      "(w (z (y x)))"))))

(output
 (lambda ()
   (let ([input (swap (swap (swap (swap (v "x") (v "y")) (v "z")) (v "w")) (v "a"))])
     (equal?
      (react input)
      "(a (w (z (y x))))"))))

(output
 (lambda ()
   (let ([input (swap (swap (swap (swap (swap (v "x") (v "y")) (v "z")) (v "w")) (v "a")) (v "b"))])
     (equal?
      (react input)
      "(b (a (w (z (y x)))))"))))