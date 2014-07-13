#lang planet neil/sicp
;Helper methods
(define (sum xs)
  (apply + xs))

;First, we must define our symbols.
(define roman-symbols 
  (list (cons 1000 #\M)
        (cons 500 #\D)
        (cons 100 #\C)
        (cons 50 #\L)
        (cons 10 #\X)
        (cons 5 #\V)
        (cons 1 #\I) ))

; The above is a convenient way to define our symbols,
; but the collection is difficult to iterate through since all lists
; in scheme are nested pairs. It becomes burdensome to check during recursion 
; whether a given 'pair' is actually a 'sublist' or an 'inner pair'.
; Instead, we create two separate lists that are easier to work with.
(define decimal-numbers
  (map car roman-symbols))

(define roman-numbers
  (map cdr roman-symbols))


;Roman -> Arabic
(define (reduce-roman-symbol-to-int symbol decimal-numbers roman-numbers)
  (if (null? decimal-numbers) 0)
  (if (char=? symbol (car roman-numbers))
      (car decimal-numbers)
      (reduce-roman-symbol-to-int symbol (cdr decimal-numbers) (cdr roman-numbers))))

(define (roman-symbol-to-int symbol)
  (reduce-roman-symbol-to-int symbol decimal-numbers roman-numbers))

(define (roman->decimal numeral)
  (sum (map roman-symbol-to-int (string->list numeral))))

;Arabic -> Roman


(define (reduce-int-to-roman int numeral)
  (if (= int 0) numeral)
  
  )



(reduce-int-to-roman 0 "")
   
  
;examples
(roman-symbol-to-int #\X)
(roman->decimal "XXV")

