#lang racket


;;; CAUTION: read VERY CAREFULLY hw5-1-grade.rkt before doing your HW.
;;; Instructions how to write submission (and grade it) is written in
;;; hw5-1-grade.rkt.

;;; If these statements are omitted, your submission will be graded 0.
;;; You can add whatever function you would like to make public.
;;; For example, if you want a function foo in hw5-2, provide it.

(provide black)
(provide white)

(provide glue-array-from-tree)
(provide glue-array-from-array)
(provide rotate-array)
(provide neighbor-array)
(provide pprint-array)
(provide is-array?)

(provide glue-tree-from-tree)
(provide glue-tree-from-array)
(provide rotate-tree)
(provide neighbor-tree)
(provide pprint-tree)
(provide is-tree?)

(provide array-to-tree)
(provide tree-to-array)

(provide glue)
(provide rotate)
(provide neighbor)
(provide pprint)


;;; primitive tile
; A black tile is 'B and a whit etile is 'W.

(define black ; black: form
  'B)
(define white ; white: form
  'W)


;;; complex tile
;
; An array tile looks like:
; (cons 'array (list row_1 row_2 ... row_n)),
; for each row_i = (list cell_i1 ... cell_in).
;
; Examples:
; 1.
; (cons 'array (list (list 'B 'B) (list 'W 'W)))
; BB
; WW
;
; 2.
; (cons 'array (list (list 'B 'B 'B 'B) (list 'B 'B 'B 'B) (list 'W 'W 'B 'B) (list 'W 'W 'B 'B)))
; BBBB
; BBBB
; WWBB
; WWBB
;
;
; An tree tile looks like:
; (cons 'tree (list subtree_nw subtree_ne subtree_se subtree_sw)).
;
; Examples:
; 1.
; (cons 'tree (list 'B 'B 'W 'B))
; BB
; BW
;
; 2.
; (cons 'tree (list (list 'B 'B 'B 'W) (list 'B 'B 'W 'B) (list 'B 'W 'B 'B) (list 'W 'B 'B 'B)))
; BBBB
; WBBW
; BWWB
; BBBB
;
; See hw5-1-grade.rkt for more details on grading array and tree representation.

(define (glue-array-from-tree nw ne se sw) ; glue-array-from-tree: form * form * form * form -> form
  (tree-to-array (glue-tree-from-tree nw ne se sw)))

(define (glue-array-from-array nw ne se sw) ; glue-array-from-array: form * form * form * form -> form
  (if (pair? nw)
      (cons 'array (append (array-append (cdr nw) (cdr ne)) (array-append (cdr sw) (cdr se))))
      (cons 'array (append (list (list nw ne)) (list (list sw se))))
      )
  )

(define (array-append a b) ; array-append: array * array -> array
  (if (and (null? (cdr a)) (null? (cdr b)))
       (list (append (car a) (car b)))
       (append (list (append (car a) (car b))) (array-append (cdr a) (cdr b)))
       )
  )


(define (glue-tree-from-tree nw ne se sw) ; glue-tree-from-tree: form * form * form * form -> form
  (if (pair? nw)
      (cons 'tree (list (cdr nw) (cdr ne) (cdr se) (cdr sw)))
      (cons 'tree (list nw ne se sw))
      )
  )

(define (glue-tree-from-array nw ne se sw) ; glue-tree-from-array: form * form * form * form -> form
  (array-to-tree (glue-array-from-array nw ne se sw)))

(define (rotate-array f) ; rotate-array: form -> form
  (tree-to-array (rotate-tree (array-to-tree f))))

(define (component index array)
  (let ((lst (cdr array)))
  (if (or (< (car index) 1) (> (car index) (length lst)) (< (cdr index) 1) (> (cdr index) (length lst)))
      'W  ;for calculating the neighbor of the component at edge.
      (list-ref (list-ref lst (- (car index) 1)) (- (cdr index) 1)))))

;convert from location to index notation.
(define (location-to-index location)
  (let ((length (expt 2 (length location))))
    (cond ((null? (cdr location)) 
           (cond ((equal? (car location) 0) (cons 1 1))
                 ((equal? (car location) 1) (cons 1 2))
                 ((equal? (car location) 2) (cons 2 2))
                 ((equal? (car location) 3) (cons 2 1))))
          ((equal? (car location) 0) (location-to-index (cdr location)))
          ((equal? (car location) 1) (cons (car (location-to-index (cdr location))) (+ (/ length 2) (cdr (location-to-index (cdr location))))))
          ((equal? (car location) 2) (cons (+ (/ length 2) (car (location-to-index (cdr location)))) (+ (/ length 2) (cdr (location-to-index (cdr location))))))
          ((equal? (car location) 3) (cons (+ (/ length 2) (car (location-to-index (cdr location)))) (cdr (location-to-index (cdr location)))))))
    )

(define (is-black? c)
  (equal? 'B c))

(define (number-of-black lst)
  (if (null? (cdr lst))
      (if (is-black? (car lst)) 1 0)
      (if (is-black? (car lst)) (+ 1 (number-of-black (cdr lst))) (number-of-black (cdr lst)))))

(define (neighbor-array location f) ; neighbor-array: location * form -> int
  (let ((i (location-to-index location)))
    (let ((neighbor-list (list (component (cons (+ -1 (car i)) (+ -1 (cdr i))) f) 
                             (component (cons (+ -1 (car i)) (+ 0 (cdr i))) f) 
                             (component (cons (+ -1 (car i)) (+ 1 (cdr i))) f) 
                             (component (cons (+ 0 (car i)) (+ -1 (cdr i))) f)
                             (component (cons (+ 0 (car i)) (+ 1 (cdr i))) f)
                             (component (cons (+ 1 (car i)) (+ -1 (cdr i))) f)
                             (component (cons (+ 1 (car i)) (+ 0 (cdr i))) f)
                             (component (cons (+ 1 (car i)) (+ 1 (cdr i))) f))))
    (number-of-black neighbor-list))))
    

; In the document, it is said to have type form -> void, but implement
; as form -> string.
; Read hw5-1-grade.rkt for formatting.

(define (fold lst f c) ; fold: (a list)*(a -> string)*string -> string
  (if (null? lst)
      c
      (f (car lst) (fold (cdr lst) f c))
      )
  )

(define (to-string lst) ; to-string: a list -> string
  (define (f a) ; f: a -> string
    (cond ((equal? a 'B) "B")
          ((equal? a 'W) "W")
          (else "\n")))
  (fold (map f lst) string-append "\n")
  )

(define (pprint-list lst) ; pprint-list: (a list) list -> string
  (if (null? (cdr lst))
      (to-string (car lst))
      (string-append (to-string (car lst)) (pprint-list (cdr lst)))
      )
  )

(define (pprint-array f) ; pprint-array: form -> string
  (let ((lst (cdr f)))
    (pprint-list lst)
    )
  )

(define (is-array? f) ; is-array?: form -> bool
  (cond [(equal? 'B f) #t]
        [(equal? 'W f) #t]
        [(equal? 'array (car f)) #t]
        [else #f]))


;;; implementation with tree

(define (fst-three lst) ; fst-three: (a list) list -> (a list) list
  (append (list (car lst) (car (cdr lst))) (list (car (cdr (cdr lst))))))

(define (lst-one lst) ; lst-three: (a list) list -> (a list) list
  (cdr (cdr (cdr lst))))

(define (rotate-list lst) ; rotate-list: (a list) list -> (a list) list
  (append (lst-one lst) (fst-three lst)))

(define (rotate-all lst) ; rotate-list: (a list) list -> (a list) list
  (if (not (list? (car lst)))
      (rotate-list lst)
      (rotate-list (list (rotate-all (car lst)) (rotate-all (car (cdr lst))) (rotate-all (car (cdr (cdr lst)))) (rotate-all (car (cdr (cdr (cdr lst)))))))
      )
  )

(define (rotate-tree f) ; rotate-tree: form -> form
  (if (not (pair? f))
      f
      (cons 'tree (rotate-all (cdr f)))
      )
  )

(define (neighbor-tree loc f) ; neighbor-tree: location * form -> int
  (neighbor-array loc (tree-to-array f)))

; In the document, it is said to have type form -> void, but implement
; as form -> string.
(define (pprint-tree f) ; pprint-tree: form -> string
  (pprint-array (tree-to-array f)))

(define (is-tree? f) ; is-tree?: form -> bool
  (cond [(equal? 'B f) #t]
        [(equal? 'W f) #t]
        [(equal? 'tree (car f)) #t]
        [else #f]))

;;; conversions 

;(array-direction lst) returns array of specific region.
;for example (array-nw (list '(B W B B) '(B B W W) '(W B W B) '(B W B W)))
;returns '((B W) (B B))
;BWBB      BW
;BBWW  ->  BB
;WBWB
;BWBW

(define (array-nw lst)
  (let [(array-n (reverse (list-tail (reverse lst) (/ (length lst) 2))))
        (f (lambda (x) (reverse (list-tail (reverse x) (/ (length x) 2)))))]
    (map f array-n)
    )
  )

(define (array-ne lst)
  (let [(array-n (reverse (list-tail (reverse lst) (/ (length lst) 2))))
        (f (lambda (x) (list-tail x (/ (length x) 2))))]
    (map f array-n)
    )
  )

(define (array-se lst)
  (let [(array-s (list-tail lst (/ (length lst) 2)))
        (f (lambda (x) (list-tail x (/ (length x) 2))))]
    (map f array-s)
    )
  )

(define (array-sw lst)
  (let [(array-s (list-tail lst (/ (length lst) 2)))
        (f (lambda (x) (reverse (list-tail (reverse x) (/ (length x) 2)))))]
    (map f array-s)
    )
  )
    
(define (array-to-tree f) ; array-to-tree: form -> form
  (if (is-tree? f)
      f
      (let ((lst (cdr f))) 
        (if (not (pair? f))
            f
            (if (= (length (car lst)) 1)
                (car (car lst))
                (glue-tree-from-tree (array-to-tree (cons 'array (array-nw lst))) (array-to-tree (cons 'array (array-ne lst))) (array-to-tree (cons 'array (array-se lst))) (array-to-tree (cons 'array (array-sw lst))))
                )
            )
        )
      )
  )

(define (tree-to-array f) ; tree-to-array: form -> form
  (if (is-array? f)
      f
      (let ((lst (cdr f)))
        (if (not (pair? f))
            f
            (if (not (list? (car lst)))
                (glue-array-from-array (list-ref lst 0) (list-ref lst 1) (list-ref lst 2) (list-ref lst 3))
                (glue-array-from-array (tree-to-array (cons 'tree (list-ref lst 0))) (tree-to-array (cons 'tree (list-ref lst 1))) (tree-to-array (cons 'tree (list-ref lst 2))) (tree-to-array (cons 'tree (list-ref lst 3))))
                )
            )
        )
      )
  )

;;; interfaces

(define (glue nw ne se sw) ; glue: form * form * form * form -> form
  (cond ((and (is-array? nw) (is-array? ne) (is-array? se) (is-array? sw)) (glue-array-from-array nw ne se sw))
        ((and (is-tree? nw) (is-tree? ne) (is-tree? se) (is-tree? sw)) (glue-tree-from-tree nw ne se sw))
        (else (glue-array-from-array (tree-to-array nw) (tree-to-array ne) (tree-to-array se) (tree-to-array sw)))
        )
  )

(define (rotate f) ; rotate: form -> form
  (if (is-array? f)
      (rotate-array f)
      (rotate-tree f)))

(define (neighbor loc f) ; neighbor: location * form -> int
  (if (is-array? f)
      (neighbor-array loc f)
      (neighbor-tree loc f)))

; In the document, it is said to have type form -> void, but implement
; as form -> string.
(define (pprint f) ; pprint: form -> string
  (if (is-array? f)
      (pprint-array f)
      (pprint-tree f)))