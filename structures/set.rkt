; This is a procedural-based representation of a set data structure and
; associated operations.

; Return a function that takes a number as an argument and tells whether or not
; that number is in the set
(define (singleton-set x)
  (lambda (n) (= x n)))

; The set of all elements that are in either 's1' or 's2'
(define (union s1 s2)
  (lambda (n) (or (s1 n) (s2 n))))

; The set of all elements that are in both 's1' and 's2'
(define (intersection s1 s2)
  (lambda (n) (and (s1 n) (s2 n))))

; The set of all elements that are in 's1' but not in 's2'
(define (diff s1 s2)
  (lambda (n) (and (s1 n) (not (s2 n)))))

; Return the subset of s for which the predicate 'predicate' is true
(define (filter predicate s)
  (lambda (n) (and (s n) (predicate n))))

; Assume that the sets can contain only numbers between 0 and bound
(define bound 100)

; Return whether or not the set contains at least an element for which the
; predicate is true
(define (exists? predicate s)
  (exists-recursive predicate s 0))

(define (exists-recursive predicate s n)
  (cond
    ((= n (+ bound 1)) #f)
    (else
      (or ((filter predicate s) n) (exists-recursive predicate s (+ n 1))))))

; Return whether or not the predicate is true for all the elements of the set
(define (all? predicate s)
  (all-recursive predicate s 0))

(define (all-recursive predicate s n)
  (cond
    ((= n (+ bound 1)) #t)
    (else
      (define intersect (intersection s (singleton-set n)))
      (cond
        ((intersect n) (and ((filter predicate s) n)
                            (all-recursive predicate s (+ n 1))))
        (else
          (all-recursive predicate s (+ n 1)))))))

; Return a new set where "op" has been applied to all elements
(define (map-set op s)
  (map-set-recursive op s (singleton-set -1) 0))

(define (map-set-recursive op s mapped n)
  (cond
    ((= n (+ bound 1)) mapped)
    (else
      (define intersect (intersection s (singleton-set n)))
      (cond
        ((intersect n) (union (map-set-recursive op s mapped (+ n 1))
                              (singleton-set (op n))))
        (else
          (map-set-recursive op s mapped (+ n 1)))))))

