
; Replace the element at index i with the given element.
(define (replace i new-el lst)
  (for/list ([idx (in-range 0 (length lst))])
    (if (= idx i)
        new-el
        (list-ref lst idx))))

