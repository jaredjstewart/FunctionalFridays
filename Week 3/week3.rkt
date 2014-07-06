#lang planet neil/sicp

(define (insert x xs)
  (if (null? xs)
      (list x)
      (if (<= x 
              (car xs))
          (cons x xs)
          (cons (car xs)
                (insert x (cdr xs))))))

(define (insertion-sort xs)
  (if (null? xs)
      nil
      (insert (car xs)
              (insertion-sort (cdr xs)))))

(insertion-sort (list 1 2 5 4 6 3 7 2 9))