#lang racket


; We auto-grade only vlencode function; other functions are not
; auto-graded.
; If this statement is omitted, your submission will be graded 0.
(provide vlencode)

; Implement vlencode. The contents provided below can be modified,
; unless the modification does not change the type and satisfies the spec.


(define (vlencode frequencies) ; vlencode: (string X int) list -> (string X (int list)) list
  (encode (make-tree (makeleaf frequencies)) '())
  )

(define (makeleaf frequencies) ; makeleaf: (string X int) list-> tree list
  (my-map1 mkleaf frequencies))

(define (mkleaf pr) ; mkleaf: (string X int) -> tree
      (if (equal? (cdr pr) 0)
      null
      (leaf (car pr) (cdr pr))
      )
)
; 1. |pr| >> |pr'| iff |pr| > |pr'| and |pr'| >= 0. 
; 2. Forall |pr| >= 0: |pr| >> |cdr pr| >> ... >> 0 is finite. 
; 3. |pr| >> |cdr pr|

(define (my-map1 f list)
  (if (null? list)
      null
      (if (equal? (f (car list)) '())
          null
          (cons (f (car list)) (my-map1 f (cdr list)))
      )
      )
  )
; 1. |list| >> |list'| iff |list| > |list'| and |list'| >= 0. 
; 2. Forall |list| >= 0: |list| >> |cdr list| >> ... >> 0 is finite. 
; 3. |list| >> |cdr list|

(define (smallest trees) ; smallest: tree list -> tree
  (cond ((null? trees) null)
        ((null? (cdr trees)) (car trees))
        (else (if (null? (cdr (cdr trees)))
                  (if (<= (rootval (car trees)) (rootval (car (cdr trees))))
                      (car trees)
                      (car (cdr trees))
                      )
                  (if (<= (rootval (car trees)) (rootval (smallest (cdr trees))))
                      (car trees)
                      (smallest (cdr trees))
                      )
                  )
              )
        )
  )
; 1. |trees| >> |trees'| iff |trees| > |trees'| and |trees'| >= 0. 
; 2. Forall |trees| >= 0: |trees| >> |cdr trees| >> ... >> 0 is finite. 
; 3. |trees| >> |cdr trees|

(define (smallest? trees) ; smallest?: tree list -> boolean
  (cond ((null? trees) null)
        ((null? (cdr trees)) #t)
        (else (if (null? (cdr (cdr trees)))
                  (if (<= (rootval (car trees)) (rootval (car (cdr trees))))
                      #t
                      #f
                      )
                  (if (<= (rootval (car trees)) (rootval (smallest (cdr trees))))
                      #t
                      #f
                      )
                  )
              )
        )
  )
; 1. |trees| >> |trees'| iff |trees| > |trees'| and |trees'| >= 0. 
; 2. Forall |trees| >= 0: |trees| >> |cdr trees| >> ... >> 0 is finite. 
; 3. |trees| >> |cdr trees|


(define (my-map f lists) ; my-map: (a -> b) * a list -> b list
  (if (null? lists)
      null
      (cons (f lists) (my-map f (cdr lists)))
      )
  )
; 1. |lists| >> |lists'| iff |lists| > |lists'| and |lists'| >= 0. 
; 2. Forall |lists| >= 0: |lists| >> |cdr lists| >> ... >> 0 is finite. 
; 3. |lists| >> |cdr lists|

;counting false numbers of list
(define (fnum list n) ; fnum: boolean list -> int
  (if (null? list)
      n
      (if (car list)
          (fnum (cdr list) n)
          (fnum (cdr list) (+ n 1))
          )
      )
  )
; 1. |lists| >> |lists'| iff |lists| > |lists'| and |lists'| >= 0. 
; 2. Forall |lists| >= 0: |lists| >> |cdr lists| >> ... >> 0 is finite. 
; 3. |lists| >> |cdr lists|

(define (fold lst f c) ; fold: a list * (a * b -> b) * b -> b
  (if (null? lst) c
      (f (car lst) (fold (cdr lst) f c))))
; 1. |lists| >> |lists'| iff |lists| > |lists'| and |lists'| >= 0. 
; 2. Forall |lists| >= 0: |lists| >> |cdr lists| >> ... >> 0 is finite. 
; 3. |lists| >> |cdr lists|

(define (alltrue? trees) ; allture?: tree list -> booelean
  (fold (my-map smallest? trees) (lambda (x y) (and x y)) #t)
  )


;tree list sorted by small val to large val.
(define (sort trees) ; sort: tree list -> tree list 
  (if (null? (cdr trees))
      trees
      (if (alltrue? trees)
          trees
          (if (smallest? trees)
              (cons (car trees) (sort (cdr trees)))
              (sort (append (cdr trees) (list (car trees))))
              )
          )
      ))
; 1. |(fnum (my-map trees))| >> |(fnum (my-map trees'))| iff |(fnum (my-map trees))| > |(fnum (my-map trees'))| and |trees'| >= 0. 
; 2. Forall |(fnum (my-map trees))| >= 0: |(fnum (my-map trees))| >> |(fnum (my-map (cdr trees)))| >> ... >> 0 is finite. 
; 3. |(fnum (my-map trees))| >> |(fnum (my-map (cdr trees)))|

;make tree by huffman algorithm (val=(rootval lhs)+(rootval rhs))
(define (make-tree trees) ; make-tree: tree list -> tree
  (let ((treelist (sort trees)))
  (if (null? treelist) null
        (if (null? (cdr treelist)) 
                (car treelist)
                (if (null? (cdr (cdr treelist)))
                    (node (car (cdr treelist)) (+ (rootval (car (cdr treelist))) (rootval (car treelist))) (car treelist))
                    (make-tree (append (list (node (car (cdr treelist)) (+ (rootval (car (cdr treelist))) (rootval (car treelist))) (car treelist))) (cdr (cdr treelist))))
                )
            )
        )
    )
  )
; 1. |trees| >> |trees'| iff |trees| > |trees'| and |trees'| >= 0. 
; 2. Forall |trees| >= 0: |trees| >> |cdr trees| >> ... >> 0 is finite. 
; 3. |trees| >> |cdr trees|

; enconding tree by rules which move left is 1(high frequency), right is 0(low frequency)
(define (encode tree l) ; tree * list -> ( String X (int list) ) list
  (if (isleaf? tree)
      (list (cons (leafstr tree) l))
      (if (isleaf? (rightsub tree))
          (append (list (cons (leafstr (rightsub tree)) (append l '(0)))) (encode (leftsub tree) (append l '(1))))
          (if (isleaf? (leftsub tree))
              (append (list (cons (leafstr (leftsub tree)) (append l '(1)))) (encode (rightsub tree) (append l '(0))))
              (append (encode (rightsub tree) (append l '(0))) (encode (leftsub tree) (append l '(1))))
          )
      )
    )
  )
; 1. |tree| >> |tree'| iff |tree| > |tree'| and |tree'| >= 0. 
; 2. Forall |tree| >= 0: |tree| >> |(leftsub trees)| >> ... >> 0 is finite. 
; 3. |tree| >> |leftsub tree|
; and also rightsub tree

; You may need the following tree interface.
(define pair cons) ; pair: a * b -> a X b
(define fst car) ; fst: a X b -> a
(define rest cdr) ; rest: a X b -> b
(define empty 'empty); empty : tree

(define (is-empty? tree) ; is-empty?: tree -> boolean
  (if (equal? tree empty)
      #t
      #f
      )
  )

(define (leaf str val) ; leaf: string * int -> tree
 
      (pair 'leaf (pair val str))
  )

(define (node lhs val rhs) ; node: tree * int * tree -> tree
  (if (is-empty? lhs)
      (pair 'r (pair val rhs))
      (if (is-empty? rhs)
          (pair 'l (pair val lhs))
          (pair 'lr (pair val (pair lhs rhs)))
          )
      )
  )
(define (isleaf? tree) ; isleaf?: tree -> bool
  (if (equal? (fst tree) 'leaf)
      #t
      #f
      )
  )

(define (leftsub tree) ; leftsub: tree -> tree
  (cond ((isleaf? tree) empty)
        ((equal? (fst tree) 'r) empty)
        ((equal? (fst tree) 'l) (rest (rest tree)))
        ((equal? (fst tree) 'lr) (fst (rest (rest tree))))
        (else empty))
  )

(define (rightsub tree) ; rightsub: tree -> tree
  (cond ((isleaf? tree) empty)
        ((equal? (fst tree) 'r) (rest (rest tree)))
        ((equal? (fst tree) 'l) empty)
        ((equal? (fst tree) 'lr) (rest (rest (rest tree))))
        (else empty))
  )

(define (leafval tree) ; leafval: tree -> int
  (if (isleaf? tree)
      (fst (rest tree))
      null
      )
  )

(define (leafstr tree) ; leftstr: tree -> string
  (if (isleaf? tree)
      (rest (rest tree))
      null
      )
  )

(define (rootval tree) ; rootval: tree -> value
  (fst (rest tree))
  )
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
