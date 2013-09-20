; Syntax-rule definitions for imperative-style loops.

; Syntax rule for writing for loops that yield a sequence of values.
(define-syntax-rule (for {var in value-range} return result)
  (map (lambda (x) (let ([var x]) result)) value-range))

; Syntax rule for writing while loops.
(define-syntax-rule (while condition body)
  (let loop ()
    (if condition
        (begin body (loop))
        0)))

