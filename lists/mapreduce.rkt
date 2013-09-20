; Useful functions for doing mapreduce-type operations.

; Custom implementation of foldr (fold right).
(define (foldrr op accumulate lst)
   (cond
    ((null? lst) accumulate)
    (else
      (op (car lst) (foldrr op accumulate (cdr lst))))))

; Apply a map operation to each element in the list then aggregate the results.
(define (map-reduce map-op reduce-op accumulate lst)
  (foldr reduce-op accumulate (map map-op lst)))

; Custom implementation of andmap.
(define (andmapp pred lst)
  (foldrr (lambda (x y) (and (pred x) y)) #t lst))

