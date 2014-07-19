#lang planet neil/sicp
(define (string-drop s n)
  (substring s n (string-length s)))