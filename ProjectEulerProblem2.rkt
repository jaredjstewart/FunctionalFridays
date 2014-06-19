#lang planet neil/sicp


(define (fib lastTwo  lastOne  total max)
      (if (or (> (+ lastOne lastTwo) max) (= (+ lastOne lastTwo) max))
      total
      (fib lastOne
       (+ lastOne lastTwo)         
            (if (= (modulo (+ lastOne lastTwo) 2) 0)
            (+ total lastOne lastTwo)
            total)
            max)))

(fib 0 1 0 4000000)


4613732