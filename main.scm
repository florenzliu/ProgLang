; Question 1 sumDigits
(define (sumDigits s)
  (sumDigitsHelper (string->list s)))

(define (sumDigitsHelper ls)    
  (if (null? ls) 0
    (+ (numeric? (car ls)) 
    (sumDigitsHelper (cdr ls)))))

(define (numeric? ch)
  (if (and (char<=? #\0 ch) (char>=? #\9 ch)) (- (char->integer ch) (char->integer #\0))
  0))

;(display (sumDigits "ab3nslg40adgv")) (newline)
;(display (sumDigits "")) (newline)
;(display (sumDigits "il0gv12d5f2")) (newline)
(display (sumDigits "ab1c2d3e54")) (newline)

; Question 2 
#|
(define lsCount ())
(define (countCodesHelper ls)
  (if (null? ls) 0
    (if (string=? (car ls) str)
      (+ 1 (countCodesHelper (cdr ls)))
      (countCodesHelper (cdr ls)))))
(display (countCodesHelper lst)) (newline)
|#


(define (countCodes s)
  (if (null? s) 0
    ((define lsCount ())
    (countCodesHelper lsCount (string->list s)))))
#|
(define (isInCountStr? lsCount ch1 ch2) 
  (if (null? lsCount) #f
    (if (string=? (car lsCount) (string ch1 ch2)) #t 
        (isInCountStr? (cdr lsCount) ch1 ch2)))) 
|#

(define (addToCount lsCount ch1 ch2) 
  (if (null? lsCount) #f
    (if (string=? (car lsCount) (string ch1 ch2)) #t 
        (addToCount (cdr lsCount) ch1 ch2)))) 

(define (countCodesHelper lsCount ls)
  (if (null? ls) 0
    (if (null? (car (cdr ls))) 0
        (if (addToCount lsCount (car ls) (car (cdr ls))) 
          ((set! lsCount (append lsCount (list (string (car ls) (car (cdr ls)))))) (display lsCount) (newline)
          (+ 1 (countCodesHelper lsCount (cdr ls))))))))

(display " finished here") (newline)

(define lst (list "AB" "IN" "CD" "IN"))
(define countryCode (list "IN" "CO" "CN"))
(display lst) (newline)
(display (countCodes "ABICDONINBIN" countryCode)) (newline)