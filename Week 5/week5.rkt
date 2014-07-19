#lang planet neil/sicp
;75 width  
;8 row max
;tower1 = [1 2]
;tower2 = [] 
;tower3 = [3]

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

(define (print-tower-line tower current_line max_height)
  (let ((empty_rows (- max_height 
                       (+ 1 (length tower)))))
    (cond ((= current_line max_height) (print-repeat "=" 25))
          ((<= current_line empty_rows) (block-print "|" 25))
          (else (print-disk (list-ref tower 
                                      (- current_line (+ 1 empty_rows)))
                            25)))))

(define (print-towers-reduce a b c current_line max_height)
  (if (> current_line max_height)
      (newline)
      (begin (newline)
             (print-tower-line a current_line max_height)
             (display " ")
             (print-tower-line b current_line max_height)
             (display " ")
             (print-tower-line c current_line max_height)
             (print-towers-reduce a b c (+ 1 current_line) max_height))))
(define (print-towers a b c max_height)
  (print-towers-reduce a b c 0 max_height))


(define (move-single-disk tower from to)
  (cond 
         ((and (= from 1) (= to 2)) 
          (list (cdr (list-ref tower 0)) 
                (cons (car (list-ref tower 0)) 
                      (list-ref tower 1)) 
                (list-ref tower 2)))
         ((and (= from 1) (= to 3))
          (list (cdr (list-ref tower 0))  
                (list-ref tower 1) 
                (cons (car (list-ref tower 0)) 
                      (list-ref tower 2))))
         ((and (= from 2 ) (= to 1))
          (list (cons (car (list-ref tower 1)) 
                      (list-ref tower 0)) 
                (cdr (list-ref tower 1)) 
                (list-ref tower 2)))
         ((and (= from 2) (= to 3))
          (list (list-ref tower 0) 
                (cdr (list-ref tower 1)) 
                (cons (car (list-ref tower 1)) 
                      (list-ref tower 2))))
         ((and (= from 3) (= to 1))
          (list (cons (car (list-ref tower 2))
                      (list-ref tower 0))
                (list-ref tower 1) 
                (cdr (list-ref tower 2))))
         ((and (= from 3) (= to 2))
          (list (list-ref tower 0) 
                (cons (car (list-ref tower 2)) 
                      (list-ref tower 1)) 
                (cdr (list-ref tower 2))))))

        
        
        
(define tower (list (list 1 2 3) (list 7) (list 4  5)))
(display tower)
(newline)
       
        
(define (towers-of-hanoi n source dest temp)
  (if (= n 1)
      (begin 
        (display "Move the disk from ")
        (display source) 
        (display " to " )
        (display dest)
        (newline)
        (set! tower (move-single-disk tower source dest))
        (display tower)
        (newline)
        )
      (begin 
        (towers-of-hanoi (- n 1) source temp dest)
        (display "Move the disk from ") 
        (display source)
        (display " to ")
        (display dest)
        (newline)
        (set! tower (move-single-disk tower source dest))
        (display tower)
        (newline)
        (towers-of-hanoi (- n 1) temp dest source))))


        
(towers-of-hanoi 2 1 3 2)      
        
        
        
        
        
        
        
        
        ; print tower
        ;(newline)
        
        (define (print-tower-reduce tower current_line max_height)
          (let ((empty_rows (- max_height 
                               (+ 1 (length tower)))))
            (if (= current_line max_height) 
                (begin (print-repeat "=" 25)
                       (newline))
                (begin 
                  (if (<= current_line empty_rows) 
                      (block-print "|" 25)
                      (print-disk (list-ref tower 
                                            (- current_line (+ 1 empty_rows)))
                                  25))
                  (newline)
                  (print-tower-reduce tower (+ 1 current_line) max_height)))))
        
        ;(print-tower-reduce (list 1 3 4) 1 7)
        ;(display "54")