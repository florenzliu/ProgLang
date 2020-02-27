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
;(display (sumDigits "ab1c2d3e54")) (newline)

; Question 2 
; define a reference list with all the country codes
(define countryCode (list "IN" "CO" "CN" "GB"))
; define a result list with non-repetitive items
(define resStrList (list ))

(define (countCodes S)
  (countCodesHelper (string->list S)))

(define (countCodesHelper ls)
  (cond ((null? ls) 0)
        ((null? (cdr ls)) 0)
        ((not (list? ls)) 0)
        (else 
          (if (isCountryCode? (string (car ls) (cadr ls)) countryCode) 
            ; when the result list is empty
            (if (null? resStrList) 
              (begin
                (set! resStrList (append resStrList (list (string (car ls) (cadr ls)))))
                (+ 1 (countCodesHelper (cdr ls)))) 
              ; when the result list is non-empty
              (if (not (isInResList? (string (car ls) (cadr ls)) resStrList)) 
                (begin
                  (set! resStrList (append resStrList (string (car ls) (cadr ls))))
                  (+ 1 (countCodesHelper (cdr ls))))
                (countCodesHelper (cdr ls))))
            (countCodesHelper (cdr ls))))))
            
; check if a string is in the reference country code list
(define (isCountryCode? ns ref)
  (if (null? ref) #f
    (if (string=? ns (car ref)) #t
      (isCountryCode? ns (cdr ref)))))

; check if a string is in the result substring list
(define (isInResList? ns resList)
  (cond ((null? resList) #f)
        ((string? resList) (string=? ns resList))
        (else (if (string=? ns (car resList)) #t
                (isInResList? ns (cdr resList))))))

;(display (countCodes "COABCOCOIN")) (newline)

; Question 3