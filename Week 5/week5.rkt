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
                             (- (* 2 num) 1))
               width))

(define (print-tower-reduce tower current_line max_height)
  (let ((empty_rows (- max_height 
                       (+ 1 (length tower)))))
    (cond ((= current_line max_height) (print-repeat "=" 25))
          ((<= current_line empty_rows) (block-print "|" 25))
          (else (print-disk (list-ref tower 
                                      (- current_line (+ 1 empty_rows)))
                            25)))))


(print-tower-line (list 2 3) 3 5)

(print-disk 3 25)
(newline)

      