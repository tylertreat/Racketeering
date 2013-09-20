; Calculates the values of Pascal's triangle.

(define (pascal n)
  (pascal-recursive '(1) n))
 
(define (pascal-recursive row n)
  (cond
    ((= n 0) '())
    (else
      (define next (map + (cons 0 row) (append row '(0))))
      (cons row (pascal-recursive next (- n 1))))))

