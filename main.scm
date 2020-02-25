(define (celcius->farenheit celcius)
  (+ (* 1.8 celcius) 32))
;(display (celcius->farenheit 0)) (newline)
;(display (celcius->farenheit 100)) (newline)

(define (factorial n)
  (if (zero? n) 1
    (* n (factorial (- n 1)))))

;(display(factorial 0)) (newline)
;(display(factorial 4)) (newline)
;(display(factorial 10)) (newline)

(define (fibonacci n)
  (if (< n 2) n
    (+ (fibonacci (- n 2))
    (fibonacci (- n 1)))))
;(display (fibonacci 10))
;(newline)


;(define (isDigit ch)
 ; (if (integer? (char->integer ch)) 1
  ;  0))
;(isDigit '3')

;(define (sumDigits s)
 ; (cond ((null? s) 0)
  ;      ((integer? (char->integer (car s))) (+ (char->integer (car s)) (sumDigits (cdr s))))
   ;     (else (sumDigits (cdr s)))))
;(sumDigits "a1")   
  ;  (+ (fibonacci (- n 2))
   ; (fibonacci (- n 1)))))


;(define (less_than a b)
;  (if (char<=? a b) #t
;      #f))