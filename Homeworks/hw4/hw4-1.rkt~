#lang racket


; We auto-grade only vlencode function; other functions are not
; auto-graded.
; If this statement is omitted, your submission will be graded 0.
(provide vlencode)


; Implement vlencode. The contents provided below can be modified,
; unless the modification does not change the type and satisfies the spec.
(define (vlencode frequencies) ; vlencode: (string X int) list -> (string X (int list)) list
  (cond
   [(equal? frequencies (list (cons "a" 5) (cons "b" 1) (cons "c" 1) (cons "d" 1)))
    (list (cons "a" (list 0)) (cons "b" (list 1 0)) (cons "c" (list 1 1 0)) (cons "d" (list 1 1 1)))]
   [(equal? frequencies (list (cons "a" 3) (cons "b" 4) (cons "c" 5) (cons "d" 6)))
    (list (cons "a" (list 0 0)) (cons "b" (list 0 1)) (cons "c" (list 1 0)) (cons "d" (list 1 1)))]
   [(equal? frequencies (list (cons "a" 3) (cons "b" 4) (cons "c" 5) (cons "d" 6) (cons "e" 0)))
    (list (cons "a" (list 0 0)) (cons "b" (list 0 1)) (cons "c" (list 1 0)) (cons "d" (list 1 1)))]
   [else (raise "TODO")]))


; You may need the following tree interface.

(define (leaf str val tree) ; leaf: string * int * tree -> tree
  (raise "TODO"))

(define (node lhs val rhs) ; node: tree * int * tree -> tree
  (raise "TODO"))

(define (isleaf? tree) ; isleaf?: tree -> bool
  (raise "TODO"))

(define (leftsub tree) ; leftsub: tree -> tree
  (raise "TODO"))

(define (rightsub tree) ; rightsub: tree -> tree
  (raise "TODO"))

(define (leafval tree) ; leafval: tree -> int
  (raise "TODO"))

(define (leafstr tree) ; leftstr: tree -> string
  (raise "TODO"))

(define (rootval tree) ; rootval: tree -> value
  (raise "TODO"))


; Write down types for each declaration and functions.
; -1 points for each wrong type annotation (-2 points top).


; You have to specify an evidence why a recursive call terminates.
; At the beginning of each recursive call, print the decreasing evidence
; as specified in the following function.
;
; The decreasing evidence should:
; 1) be a nonnegative integer;
; 2) be a function of arguments;
; 3) strictly decrease for each recursive call (equal numbers are not allowed).
;
; At the beginning of each recursive function you implement,
; write down, for example,
; "(printf "decreasing: ~s~n" (+ x x y))".
;
; For example, gcd satisfy the following conditions:
; 1) (+ x x y) >= 0;
; 2) (+ x x y) is a function of arguments x and y;
; 3) (+ x x y) strictly decreases for each recursive call.
(define (gcd x y)           ; int -> int -> int
  (printf "decreasing: ~s~n" (+ x x y)) ; (+ x y) strictly decreases for each recursive call.
  (cond
   [(equal? x 0) y]
   [(equal? y 0) x]
   [else (gcd (remainder y x) x)]))
; It actually decreases. Try:
(gcd 43254 193423)
