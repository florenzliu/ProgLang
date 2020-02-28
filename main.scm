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
(define countryCode (list "AD" "AE" "AF" "AG" "AI" "AL" "AM" "AO" "AQ" "AR" "AS" "AT" "AU" "AW" "AX" "AZ" "BA" "BB" "BD" "BE" "BF" "BG" "BH" "BI" "BJ" "BL" "BM" "BN" "BO" "BQ" "BR" "BS" "BT" "BV" "BW" "BY" "BZ" "CA" "CC" "CD" "CF" "CG" "CH" "CI" "CK" "CL" "CM" "CN" "CO" "CR" "CU" "CV" "CW" "CX" "CY" "CZ" "DE" "DJ" "DK" "DM" "DO" "DZ" "EC" "EE" "EG" "EH" "ER" "ES" "ET" "FI" "FJ" "FK" "FM" "FO" "FR" "GA" "GB" "GD" "GE" "GF" "GG" "GH" "GI" "GL" "GM" "GN" "GP" "GQ" "GR" "GS" "GT" "GU" "GW" "GY" "HK" "HM" "HN" "HR" "HT" "HU" "ID" "IE" "IL" "IM" "IN" "IO" "IQ" "IR" "IS" "IT" "JE" "JM" "JO" "JP" "KE" "KG" "KH" "KI" "KM" "KN" "KP" "KR" "KW" "KY" "KZ" "LA" "LB" "LC" "LI" "LK" "LR" "LS" "LT" "LU" "LV" "LY" "MA" "MC" "MD" "ME" "MF" "MG" "MH" "MK" "ML" "MM" "MN" "MO" "MP" "MQ" "MR" "MS" "MT" "MU" "MV" "MW" "MX" "MY" "MZ" "NA" "NC" "NE" "NF" "NG" "NI" "NL" "NO" "NP" "NR" "NU" "NZ" "OM" "PA" "PE" "PF" "PG" "PH" "PK" "PL" "PM" "PN" "PR" "PS" "PT" "PW" "PY" "QA" "RE" "RO" "RS" "RU" "RW" "SA" "SB" "SC" "SD" "SE" "SG" "SH" "SI" "SJ" "SK" "SL" "SM" "SN" "SO" "SR" "SS" "ST" "SV" "SX" "SY" "SZ" "TC" "TD" "TF" "TG" "TH" "TJ" "TK" "TL" "TM" "TN" "TO" "TR" "TT" "TV" "TW" "TZ" "UA" "UG" "UM" "US" "UY" "UZ" "VA" "VC" "VE" "VG" "VI" "VN" "VU" "WF" "WS" "YE" "YT" "ZA" "ZM" "ZW"))

; define a result list with non-repetitive items
(define resStrList (list ))

(define (countCodes S)
  (set! resStrList (list ))
  (countCodesHelper (string->list S)))

(define (countCodesHelper ls)
  (cond ((null? ls) 0)
        ((null? (cdr ls)) 0)
        (else 
          (if (isCountryCode? (string (car ls) (cadr ls)) countryCode) 
            ; when the current string is a country code
            (if (not (isInResList? (string (car ls) (cadr ls)) resStrList))
              ; when the current string is not in the result list 
              (begin
                (set! resStrList (append resStrList (list (string (car ls) (cadr ls)))))
                (+ 1 (countCodesHelper (cdr ls))))
              (countCodesHelper (cdr ls)))
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

;(display (countCodes "AINBCO")) (newline)

; Question 3
; define a list with unique characters
(define uniqueCharList (list ))
; define the maximum length of the substring with unique characters
(define maxLen 0)

(define (uniqueSubstring s)
  (set! uniqueCharList (list ))
  (set! maxLen 0)
  (uniqueSubstringHelper (string->list s)))

; return the maximum length of the substring with unique characters
(define (uniqueSubstringHelper ls)
  (if (null? ls) maxLen
      (begin 
        (set! uniqueCharList (list ))
        ; update the maxLen by calling currUniqSubLen function
        (set! maxLen (max maxLen (currUniqSubLen ls)))
        (max maxLen (uniqueSubstringHelper (cdr ls))))))

; return the length of the substring with unique characters that starts from first character of the list
(define (currUniqSubLen ls)
  (cond ((null? ls) 0)
        ((char? ls) (if (isInPrevStr? ls uniqueCharList) 0 1))
        (else (if (isInPrevStr? (car ls) uniqueCharList) 0
                (begin
                  (set! uniqueCharList (append uniqueCharList (list (car ls))))
                  (+ 1 (currUniqSubLen (cdr ls))))))))

(define (isInPrevStr? ch charList)
  (cond ((null? charList) #f)
        ((char? charList) (char=? ch charList))
        (else (if (char=? ch (car charList)) #t
                (isInPrevStr? ch (cdr charList))))))

;(uniqueSubstring "AAEABCDAAABDKCS")