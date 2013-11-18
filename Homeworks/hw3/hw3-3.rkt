#lang racket

(require "hw3-3-library.rkt")

(provide mazeGen)
;숙제에서 하라는데로 함수를 구현해 보았습니다만 실제 해보면 그리 예쁘고 복잡하게 미로가 만들어지진 않습니다. 
;실제 숙제에 있는 그림처럼 복잡하게끔 만들기 위해서는 제 코드를 어떻게 수정해야 할까요?
;구체적인 함수를 만들어서 제시해주시면 2점
;그냥 추상적으로 이렇게 하면 될거 같다하시면 1점 드리겠습니다.


;(open-num n) 함수는 북, 동북, 남서, 남, 남북, 동북을 시계방향 순서대로 숫자 0 1 2 3 4 5 에 대응 시킨 함수다. 
;(n>6이 되면 remainder로 나눠서 나머지값으로 처리)

;(two-exit n m i j) 함수는 (init-maze n m) 에다가 입구와 출구를 각각 하나씩 북쪽과 남쪽에 만들어주는 함수다.

;(nextlist lst n) 함수는 ((open-num n) i j maze)가 실행됫을때 오픈된 방으로 이동하는 함수로써 리스트를 반환한다. 
;방의 위치는 ((car lst), (car (cdr lst)))다. open-num 함수의 n값과 대응하여 각 open-d 함수에 대하여 이동한다. 
;이떄 미로가 6각형이므로 (car l)가 홀수일 때는 조건에 맞춰서 방의 좌표를 정한다.

(define (open-num n)
  (let ((m (remainder n 6)))
  (cond ((= m 0) open-n)
        ((= m 1) open-ne)
        ((= m 2) open-se)
        ((= m 3) open-s)
        ((= m 4) open-sw)
        ((= m 5) open-nw))
    )
  )

(define (two-exit n m i j)
  (let ((maze0 (init-maze n m)) )
    (open-s j (- m 1) (open-n i 0 maze0)))
  )

(define (nextlist lst n)
  (let ((m (remainder n 6)))
    (cond ((= m 0) (list (car lst) (- (car (cdr lst)) 1)))
        ((= m 1) (if (odd? (car lst))
                     (list (+ (car lst) 1) (- (car (cdr lst)) 1))
                     (list (+ (car lst) 1) (car (cdr lst)))))
        ((= m 2) (if (odd? (car lst))
                     (list (+ (car lst) 1) (car (cdr lst)))
                     (list (+ (car lst) 1) (+ (car (cdr lst)) 1))))
        ((= m 3) (list (car lst) (+ (car (cdr lst)) 1)))
        ((= m 4) (if (odd? (car lst))
                     (list (- (car lst) 1) (car (cdr lst)))
                     (list (- (car lst) 1) (+ (car (cdr lst)) 1))))
        ((= m 5) (if (odd? (car lst))
                     (list (- (car lst) 1) (- (car (cdr lst)) 1))
                     (list (- (car lst) 1) (car (cdr lst)))))
        ))
  )
;입구에서 임의로 방향을 정해서 방을 오픈시키는 함수를 구현하기 위해서
;(random n)함수를 통해 i 와 j를 정하여 방의 입구와 출구를 임의로 선택한다.

;그 다음으로 (mazing lst endl maze1)에서 endl 에는 출구를 대입하고 lst와 endl가 일치할때 까지 방을 입구에서 임의의 방향으로 오픈시킨다.
;이때 방의 위치가 모서리 부분에 있을 떄에만 조건을 설정하여 오픈시키는 방향을 제한한다.

(define (mazeGen n m)
  (define i (random n))
  (define j (random n))
  (define maze (two-exit n m i j))
  (define (mazing lst endl maze1)
    (if (equal? lst endl) 
        maze1
        (cond ((equal? (car lst) 0) 
               
               (if (= (car (cdr lst)) 0)
                   (let ((d (+ (random 3) 1))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1)))
                   (if (= (car (cdr lst)) (- m 1))
                       (let ((d (random 2))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1)))
                       (let ((d (random 4))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1))))))
              
              ((equal? (car lst) (- n 1))
               
               (if (= (car (cdr lst)) 0)
                  (if (odd? n)
                      (let ((d (+ (random 3) 3))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1)))
                      (let ((d (+ (random 2) 3))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1))))
                  (if (= (car (cdr lst)) (- m 1))
                      (if (odd? n)
                          (let ((d (+ (random 2) 5))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1)))
                          (let ((d (+ (random 3) 4))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1))))
                      (let ((d (+ (random 4) 3))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1))))))
              
              ((and (equal? (car (cdr lst)) 0) (> (car lst) 0) (< (car lst) (- n 1)))
               
               (if (odd? (car lst))
                   (let ((d (+ (random 3) 2))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1)))
                   (let ((d (+ (random 5) 1))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1)))))
              
              ((and (equal? (car (cdr lst)) (- m 1)) (> (car lst) 0) (< (car lst) (- n 1)))
               
               (if (odd? (car lst))
                   (let ((d (+ (random 5) 4))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1)))
                   (let ((d (+ (random 3) 5))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1)))))
              
              (else (let ((d (random 6))) (mazing (nextlist lst d) endl ((open-num d) (car lst) (car (cdr lst)) maze1)))))
        )
    )
  (mazing (list i 0) (list j (- m 1)) maze)
  )