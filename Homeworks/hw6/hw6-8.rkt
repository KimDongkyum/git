#lang racket

(require "hw6-8-library.rkt")

;;; If these statements are omitted, your submission will be graded 0.
(provide shoppingList)

;;; This problem has restrictions on the interface of shoppingList.
;;; 1. All ids are symbols consisting of capital alphabets ('A, 'B, 'DFSF, ...).
;;; 2. All gifts are integers (1, 2, 3, 100, ...).
;;; 3. The output of shoppingList should be sorted by dictionary order of id.
;;;    For example, A < AA < AB < B.
;;; 4. Each gift list in the output of shoppingList should be sorted by increasing order.
;;;
;;; See hw6-8-grade.rkt for a grading example.
;;;
(define (shoppingList conditions) ; (id X cond) list -> (id X gift list) list
  'TODO)
