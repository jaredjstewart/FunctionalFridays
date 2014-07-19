#lang planet neil/sicp
;75 width  
;8 row max
;tower1 = [1 2]
;tower2 = [] 
;tower3 = [3]
(define towers (list ))

(define (string-repeat-reduce msg buffer n)
  (if (= n 1)
      buffer
      (string-repeat-reduce msg 
                            (string-append buffer msg)
                            (- n 1))))

(define (string-repeat msg n)
  (string-repeat-reduce msg msg n))

(define (print-repeat msg n) 
  (if (= n 1)
      (display msg)
      (begin (display msg)
             (print-repeat msg (- n 1)))))

(define (print-space n)
  (print-repeat " " n))

(define (block-print msg width)
  (let ((front_padding (quotient (- width (string-length msg)) 
                                 2)))
    (begin (display (string-repeat " " front_padding))
           (display msg)
           (display (string-repeat " " (- width 
                                          (+ (string-length msg) front_padding)))))))
(define (print-disk num width)
  (block-print (string-repeat (number->string num)
                              (+ (* 2 num) 1))
               width))

(define (print-tower-line tower current_line max_height max_width)
  (let ((empty_rows (- max_height 
                       (+ 1 (length tower)))))
    (cond ((= current_line max_height) (print-repeat "=" max_width))
          ((<= current_line empty_rows) (block-print "|" max_width))
          (else (print-disk (list-ref tower 
                                      (- current_line (+ 1 empty_rows)))
                            max_width)))))

(define (print-towers-reduce a b c current_line max_height max_width)
  (if (> current_line max_height)
      (newline)
      (begin (newline)
             (print-tower-line a current_line max_height max_width)
             (display " ")
             (print-tower-line b current_line max_height max_width)
             (display " ")
             (print-tower-line c current_line max_height max_width)
             (print-towers-reduce a b c (+ 1 current_line) max_height max_width))))
(define (print-towers towers)
  (print-towers-reduce (list-ref towers 0) (list-ref towers 1) (list-ref towers 2) 0  (height towers) (width towers)))

(define (move-single-disk towers from to)
  (cond 
    ((and (= from 1) (= to 2)) 
     (list (cdr (list-ref towers 0)) 
           (cons (car (list-ref towers 0)) 
                 (list-ref towers 1)) 
           (list-ref towers 2)))
    ((and (= from 1) (= to 3))
     (list (cdr (list-ref towers 0))  
           (list-ref towers 1) 
           (cons (car (list-ref towers 0)) 
                 (list-ref towers 2))))
    ((and (= from 2 ) (= to 1))
     (list (cons (car (list-ref towers 1)) 
                 (list-ref towers 0)) 
           (cdr (list-ref towers 1)) 
           (list-ref towers 2)))
    ((and (= from 2) (= to 3))
     (list (list-ref towers 0) 
           (cdr (list-ref towers 1)) 
           (cons (car (list-ref towers 1)) 
                 (list-ref towers 2))))
    ((and (= from 3) (= to 1))
     (list (cons (car (list-ref towers 2))
                 (list-ref towers 0))
           (list-ref towers 1) 
           (cdr (list-ref towers 2))))
    ((and (= from 3) (= to 2))
     (list (list-ref towers 0) 
           (cons (car (list-ref towers 2)) 
                 (list-ref towers 1)) 
           (cdr (list-ref towers 2))))))

(define (height towers)
  (apply max (flatten towers)))
(define (width towers)
  (let ((max_width (+ (* 2 (height towers)) 1)))
    (if (< max_width 25)
        25
        (+ 6 max_width))))

(define (flatten list)
  (cond ((null? list) nil)
        ((list? (car list)) (append (flatten (car list)) (flatten (cdr list))))
        (else
         (cons (car list) (flatten (cdr list))))))

(define (make-range a b)
  (if (> a b)
      nil
      (cons a (make-range (+ 1 a) b))))

(define (make-tower n)
  (if (> n 9)
      (begin (display "Tower is too large to display.")
             (nil))
      (list (make-range 1 n)
            (list )
            (list ))))

(define (towers-of-hanoi n source temp dest)
  (if (= n 1)
      (begin 
        (display "Move the disk from ")
        (display source) 
        (display " to " )
        (display dest)
        (newline)
        (set! towers (move-single-disk towers source dest))
        (print-towers towers)
        (newline)
        )
      (begin 
        (towers-of-hanoi (- n 1) source  dest temp)
        (display "Move the disk from ") 
        (display source)
        (display " to ")
        (display dest)
        (newline)
        (set! towers (move-single-disk towers source dest))
        (print-towers towers)
        (newline)
        (towers-of-hanoi (- n 1) temp source dest))))

(define (demo height)
  (begin(newline)
        (display "Starting configuration with ")
        (display height)
        (display " disks")
        (newline)
        (set! towers (make-tower height))
        (newline)
        (print-towers towers)
        (newline)
        (towers-of-hanoi height 1 2 3)))
(demo 8)

;(print-tower-reduce (list 1 3 4) 1 7)
;(display "54")
;todo (define (height towers) 