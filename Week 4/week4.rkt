#lang planet neil/sicp
;Helper methods
(define (emptyOrNull str) 
  (or (null? str)
      ( = (string-length str) 0)))

(define (sum xs)
  (apply + xs))

(define filterb
    (lambda (pred lst)
      (cond ((null? lst) '())
            ((pred (car lst)) (cons (car lst) (filterb pred (cdr lst))))
            (else (filterb pred (cdr lst))))))

(define get-val car)
(define get-symbol cdr)

;Conversion
(define roman-symbols 
  (list (cons 1 #\I)
        (cons 5 #\V)
        (cons 10 #\X)
        (cons 50 #\L)
        (cons 100 #\C)
        (cons 500 #\D)
        (cons 1000 #\M)))

;Roman -> Arabic
(define (reduce-roman-symbol-to-int symbol symbols)
  (cond ((null? symbols) 
         0)
        ((char=? symbol (get-symbol (car symbols))) 
         (get-val (car symbols)))
        (else (reduce-roman-symbol-to-int symbol (cdr symbols)))
        ))

(define (roman-symbol-to-int symbol) 
  (reduce-roman-symbol-to-int symbol roman-symbols)) 

(define (roman-numeral-to-arabic numer)
  (sum (map roman-symbol-to-int (string->list numer))))

;Arabic -> Roman


(define (reduce-int-to-roman int numeral)
  (if (= int 0) numeral)
  
  )



(reduce-int-to-roman 0 "")
   
  
;examples
(roman-symbol-to-int #\X)
(roman-numeral-to-arabic "XXV")

