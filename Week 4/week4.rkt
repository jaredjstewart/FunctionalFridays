#lang planet neil/sicp
;Helper methods
(define (emptyOrNull str) 
  (or (null? str)
      ( = (string-length str) 0)))

(define (sum xs)
  (apply + xs))
;Conversion
(define (roman-symbol-to-int symbol)
  (cond
    ((char=? symbol #\I) 1)
    ((char=? symbol #\V) 5)
    ((char=? symbol #\X) 10)
    ((char=? symbol #\L) 50)
    ((char=? symbol #\C) 100)
    ((char=? symbol #\D) 500)
    ((char=? symbol #\M) 1000)
    ))

(define (roman-to-arabic numeralString)
    (sum (map roman-symbol-to-int
                (string->list numeralString))))




(roman-to-arabic "XXV")

