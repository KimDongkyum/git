#lang racket

(require "common-grade.rkt")
(require "hw1-3.rkt")

(define (yanghuiGrade)
	(begin
		(printf "yanghui\n")
        (output (equal? "11112113311464115101051161520156117213535217118285670562881193684126126843691" (yanghui 10)))
        (output (equal? "11112113311464115101051161520156117213535217118285670562881" (yanghui 9)))
        (output (equal? "111121133114641151010511615201561172135352171" (yanghui 8)))
        (output (equal? "111121133114641151010511615201561" (yanghui 7)))
        (output (equal? "11112113311464115101051" (yanghui 6)))
        (output (equal? "111121133114641" (yanghui 5)))
        (output (equal? "1111211331" (yanghui 4)))
        (output (equal? "111121" (yanghui 3)))
        (output (equal? "111" (yanghui 2)))
        (output (equal? "1" (yanghui 1)))
        (output (equal? "" (yanghui 0)))
        (output (equal? "1" (yanghui -1)))
        (output (equal? "111" (yanghui -2)))))

(yanghuiGrade)
