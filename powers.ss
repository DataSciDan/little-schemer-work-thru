; I wrote this as a language-agnostic reminder of how to write a power function

(define power      ; timewise: O(n) spacewise: O(n), but there is a way to make it O(1) space in other languages
        (lambda (base exp)
                (cond
                        ((zero? exp) 1)
                        (else (* base (power base (- exp 1))))
                )
        )
)

(define power2     ; timewise: O(log n) spacewise: O(log n)
        (lambda (base exp)
                (cond
                        ((zero? exp) 1)
                        ((zero? (modulo exp 2)) (power2 (* base base) (/ exp 2)))
                        (else (* base (power2 base (- exp 1))))
                )
        )
)
