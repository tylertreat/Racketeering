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

; Reduce an NxM matrix to a vector of length M by applying an operation to
; the values of every column.
(define (matrix-to-vector op mat)
  (define result '())
  (for/list ([col (in-range 0 (length (car mat)))])
    (append result (apply op (reverse (column-values mat col '()))))))

; Retrieve the column values from a matrix for a specific column.
(define (column-values mat col vals)
  (cond
    ((null? mat) vals)
    (else
      (column-values (cdr mat) col (cons (list-ref (car mat) col) vals)))))

