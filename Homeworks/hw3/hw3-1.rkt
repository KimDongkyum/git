 #lang racket

; DO NOT ADD OTHER COMMENTS.
; MODIFY ONLY "TODO"s in the following comments.

(define (gcd n m) ; gcd: int*int -> int, n,m: int
  (if (equal? n 0) m
      (gcd (remainder m n) n)))

(define (zpn x) ; zpn: crazy2 -> int, x: crazy2
  (cond ((equal? x 'z) 0)
        ((equal? x 'p) 1)
        ((equal? x 'n) -1)))

(define (crazy2val c) ; crazy2val: crazy2 -> int, c: crazy2
  (cond ((pair? c)
         (let ((hd (car c)) ; hd: crazy2
               (tl (cdr c))) ; tl: crazy2
           (+ (* 2 (crazy2val tl)) (zpn hd))))
        (else (zpn c))))

(define (zpnadd a b c) ; zpnadd: crazy2*crazy2*crazy2 -> crazy2, a,b,c: crazy2
  (let ((sum (+ (zpn a) (zpn b) (zpn c)))) ; sum: int
    (cond ((equal? sum 3) (cons 'p 'p))
          ((equal? sum 2) (cons 'z 'p))
          ((equal? sum 1) (cons 'p 'z))
          ((equal? sum 0) (cons 'z 'z))
          ((equal? sum -1) (cons 'n 'z))
          ((equal? sum -2) (cons 'z 'n))
          ((equal? sum -3) (cons 'n 'n)))))

(define (crazy2add-carry lhs rhs carry) ; crazy2add-carry: crazy2*crazy2*crazy2 -> crazy2, lhs,rhs,carry: crazy2
  (if (pair? lhs)
      (let ((lhd (car lhs)) ; lhd: crazy2
            (ltl (cdr lhs))) ; ltl: crazy2
        (if (pair? rhs)
            (let* ((rhd (car rhs)) ; rhd: crazy2
                  (rtl (cdr rhs)) ; rtl: crazy2
                  (sum (zpnadd lhd rhd carry))) ; sum: crazy2
              (cons (car sum) (crazy2add-carry ltl rtl (cdr sum))))
            (let ((sum (zpnadd lhd rhs carry))) ; sum: crazy2
              (cons (car sum) (crazy2add-carry ltl 'z (cdr sum))))))
      (if (pair? rhs)
          (let* ((rhd (car rhs)) ; rhd: crazy2
                (rtl (cdr rhs)) ; rtl: crazy2
                (sum (zpnadd lhs rhd carry))) ; sum: crazy2
            (cons (car sum) (crazy2add-carry 'z rtl (cdr sum))))
          (zpnadd lhs rhs carry))))

(define (crazy2add lhs rhs) ; crazy2add: crazy2*crazy2 -> crazy2, lhs,rhs: crazy2
  (crazy2add-carry lhs rhs 'z))

(define (zipper lhs rhs) ; zipper: (int list)*(int list) -> (int list), lhs,rhs: int list
  (if (pair? lhs)
      (let ((lhd (car lhs)) ; lhd: int
            (ltl (cdr lhs))) ; ltl: int list
        (if (pair? rhs)
            (let ((rhd (car rhs)) ; rhd: int
                  (rtl (cdr rhs))) ; rtl: int list
              (cons lhd (cons rhd (zipper ltl rtl))))
            lhs))
      rhs))

(define (zipperN lists) ; zipperN: (int list) list -> int list, lists: (int list) list
  (if (pair? lists)
      (let ((lhd (car lists)) ; lhd: int list
            (ltl (cdr lists))) ; ltl: (int list) list
        (if (pair? lhd)
            (let ((llhd (car lhd)) ; llhd: int
                  (lltl (cdr lhd))) ; lltl: int list
              (cons llhd (zipperN (append ltl (list lltl)))))
            (zipperN ltl)))
      '()))

(define (iter n f) ; iter: int*(real -> real) -> (real -> real), n: int, f: real -> real
  (let ((m (abs n))) ; m: int
    (if (equal? m 0)
        (lambda (x) x)
        (lambda (x)
          (let ((fx (f x)) ; fx: real -> real
                (remf (iter (- m 1) f))) ; remf: (real -> real) -> (real -> real)
            (remf fx))))))
