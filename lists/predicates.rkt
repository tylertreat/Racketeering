; Some useful predicate list operators.

; Indicate if the list contains only even numbers.
(define (list-of-even-numbers? lst)
  (if (not (list? lst))
      #f
      (if (null? lst)
          #t
          (and
           (and
            (number? (car lst))
            (even? (car lst)))
           (list-of-even-numbers? (cdr lst))))))

; Indicate if the list contains only values satisfying the predicate.
(define (list-of-all? predicate lst)
  (if (not (list? lst))
      #f
      (if (null? lst)
          #t
          (and
           (predicate (car lst))
           (list-of-all? predicate (cdr lst))))))

