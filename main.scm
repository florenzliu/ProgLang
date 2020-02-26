; Question 1 sumDigits
(define (sumDigits s)
  (sumDigitsHelper (string->list s)))

(define (sumDigitsHelper ls)    
  (if (null? ls) 0
    (+ (numeric? (car ls)) 
    (sumDigitsHelper (cdr ls)))))

(define (numeric? ch)
  (if (and (char<=? #\0 ch) (char>=? #\9 ch)) 
  (- (char->integer ch) (char->integer #\0))
  0))

;(display (sumDigits "ab3nslg40adgv")) (newline)
;(display (sumDigits "")) (newline)
;(display (sumDigits "il0gv12d5f2")) (newline)
(display (sumDigits "ab1c2d3e54")) (newline)

; Question 2 
