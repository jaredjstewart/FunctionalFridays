#lang planet neil/sicp


(define (fib previousTerm  currentTerm  total max)   
      (define nextTerm (+ currentTerm previousTerm))

      (if (or (> nextTerm max) (= nextTerm max))
      total
      (fib currentTerm
       nextTerm         
            (if (= (modulo nextTerm 2) 0)
            (+ total nextTerm)
            total)
            max)))

(fib 0 1 0 4000000)


4613732