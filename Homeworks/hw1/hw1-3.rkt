#lang racket

(provide yanghui)

(define (pascalNumber n m) ; define pascalNumber function which returns the integer number of n-th column, m-th row of pascaltriangle 
  (if (= n 0) ; initial condition
      1
      (if (= m 0) ;initial condition
          1
          (if (= m n)
              1
              (+ (pascalNumber (- n 1) (- m 1 )) (pascalNumber (- n 1) m)) ;the property of the pascaltriangle
              )
          )
      )
  )

(define (pascalString n ) ; define pascalString function which returns the string of n-th column of pascaltriangle
  (define ( argString n m) ; define argString function which helps to make pascalString function
    (if (= m 0) ; initial condition
        "1"
            (if (> n m)
                (string-append (number->string (pascalNumber n m)) (argString n (- m 1)) ) 
                (string-append "1" (argString n (- n 1)) ) ; if m=n
                )
            )
        )
  (argString n n)
  )
            
            
(define (yanghui n)
  (if (< n 0) ; for negative n integer
      (yanghui (* n -1)) ; make n absolute value
      (if (= n 0) ; initial condition
          ""
          (if (= n 1) ; initial condition
              "1"
              (string-append (yanghui (- n 1)) (pascalString (- n 1))) ; called yanghui function recursively
              )
          )
      )
  )
  
  

