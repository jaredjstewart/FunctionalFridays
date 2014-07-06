#lang planet neil/sicp

;;;;;;;;;;;;;;;; occursIn ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (occursIn? x xs)
  (cond ((null? xs) false)
        ((equal? x (car xs)) true)
        (else (occursIn? x (cdr xs)))))
;examples
(occursIn? 5 (list 1 2 3 4 5)) ;#t
(occursIn? 5 (list 1 2 3 4 6)) ;#f

;;;;;;;;;;;;;;;;; allOccursIn ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (allOccursIn xs ys)
  (not (occursIn? false 
                 (map (lambda(x) (occursIn? x ys)) 
                      xs))))

;examples
(allOccursIn (list 1 2) (list 1 2 3)) ;#t
(allOccursIn (list 1 2) (list 2 3)) ;#f

;;;;;;;;;;;;;;;;; sameElements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sameElements xs ys)
  (and (allOccursIn xs ys)
       (allOccursIn ys xs)))

;examples
(sameElements (list 2 3) (list 3 2)) ;#t
(sameElements (list 2 3) (list 2)) ;#f 

;;;;;;;;;;;;;;;;; numOccurences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;first, we implement sumList:
(define (sumList xs)
  (if (null? xs) 0
      (+ (car xs)
         (sumList (cdr xs)))))

;then we use sumList along with map to compute numOccurences:
(define (numOccurences x xs)
  (sumlist (map (lambda(y) (if (= x y) 
                               1
                               0))
                xs)))

;examples
(sumlist (list 1 1 1 2 3)) ;8
(define sampleList (list 1 2 3 1 2 3 4 5 1))
(numOccurences 1 sampleList) ;3
(numOccurences 2 sampleList) ;2
(numOccurences 4 sampleList) ;1
(numOccurences 6 sampleList) ;0

;;;;;;;;;;;;;;;;; toSet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;first we define methods implementing a simple representation of sets as unordered lists
(define (elementOfSet? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (elementOfSet? x (cdr set)))))

(define (addElementToSet x set)
  (if (elementOfSet? x set)
      set
      (cons x set)))
;example
(define mySet1 (addElementToSet 1 nil)) ;{1}
(define mySet2 (addElementToSet 2 mySet1)) ;{1,2}

;now we can define an iterative list -> set method
(define (iterListToSet myList mySet)
  (if (null? myList) mySet
      (iterListToSet (cdr myList)
                     (addElementToSet (car myList)
                                      mySet))))
;and curry the default parameter of mySet = {} for the desired method
(define (toSet myList) 
  (iterListToSet myList nil))

;example
(toSet sampleList) ; {1 2 3 4 5}


