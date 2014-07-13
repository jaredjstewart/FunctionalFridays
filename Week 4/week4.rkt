#lang planet neil/sicp
;Helper methods
(define (sum xs)
  (apply + xs))

(define filterb
  (lambda (pred lst)
    (cond ((null? lst) '())
          ((pred (car lst)) (cons (car lst) (filterb pred (cdr lst))))
          (else (filterb pred (cdr lst))))))

(define (contains x xs)
  (if (null? xs)
      #f
      (if (string=? x (car xs))
          #t
          (contains x (cdr xs)))))

(define (string-drop s n)
  (substring s n (string-length s)))
;First, we must define our symbols.
(define roman-symbols 
  (list (cons 1000 "M")
        (cons 900 "CM")
        (cons 500 "D")
        (cons 400 "CD")
        (cons 100 "C")
        (cons 90 "XC")
        (cons 50 "L")
        (cons 40 "XL")
        (cons 10 "X")
        (cons 9 "IX")
        (cons 5 "V")
        (cons 4 "IV")
        (cons 1 "I") ))

; The above is a convenient way to define our symbols,
; but the collection is difficult to iterate through since all lists
; in scheme are nested pairs. It becomes burdensome to check during recursion 
; whether a given 'pair' is actually a 'sublist' or an 'inner pair'.
; Instead, we create two separate lists that are easier to work with.
(define decimal-numbers
  (map car roman-symbols))

(define roman-numbers
  (map cdr roman-symbols))

; and we implement an interface to these representations
(define (compound-symbol? x) 
  (contains x (filterb (lambda(x) (= 2 (string-length x))) roman-numbers)))

;Roman->Decimal conversion
(define (reduce-roman-symbol-to-int symbol decimal-numbers roman-numbers)
  (if (null? decimal-numbers) 0
      (if  (string=? symbol (car roman-numbers))
           (car decimal-numbers)
           (reduce-roman-symbol-to-int symbol (cdr decimal-numbers) (cdr roman-numbers)))))

(define (roman-symbol-to-int symbol)
  (reduce-roman-symbol-to-int symbol decimal-numbers roman-numbers))

(define (reduce-roman-numeral-to-int s decimal)
    (if (null? decimal-numbers) decimal
        (if (< (string-length s) 2) 
            (+ decimal (roman-symbol-to-int s))
            (if (compound-symbol? (substring s 0 2)) 
                (reduce-roman-numeral-to-int (string-drop s 2) 
                                             (+ decimal (roman-symbol-to-int (substring s 0 2))))
                (reduce-roman-numeral-to-int (string-drop s 1)
                                             (+ decimal (roman-symbol-to-int (substring s 0 1 ))))))))

(define (roman->decimal numeral)
    (reduce-roman-numeral-to-int numeral 0))

;Decimal->Roman conversion
(define (reduce-int-to-roman decimal s decimal-numbers roman-numbers)
  (if (null? decimal-numbers)
      s
      (if (>= decimal (car decimal-numbers))
          (reduce-int-to-roman (- decimal (car decimal-numbers)) 
                               (string-append s (car roman-numbers)) 
                               decimal-numbers roman-numbers)
          (reduce-int-to-roman decimal s 
                               (cdr decimal-numbers) (cdr roman-numbers)))))
(define (decimal->roman number)
  (reduce-int-to-roman number "" decimal-numbers roman-numbers))


;examples
(roman-symbol-to-int "IX")
(roman->decimal "MMXIV")
(decimal->roman 2014)
(decimal->roman 44)