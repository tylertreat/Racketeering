; Implementation of the merge sort algorithm.

(define (sort lst comp)
 (cond
    ((or (null? lst) (null? (cdr lst))) lst)
    (else
      (merge (cons (car lst) (list)) (sort (cdr lst) comp) comp))))

(define (merge list-a list-b comp)
  (cond
    ((null? list-a) list-b)
    ((null? list-b) list-a)
    (else
      (if (comp (car list-a) (car list-b))
          (cons (car list-a) (merge (cdr list-a) list-b comp))
          (cons (car list-b) (merge (cdr list-b) list-a comp))))))

