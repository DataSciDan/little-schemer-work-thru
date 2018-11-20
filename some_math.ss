#lang racket
(define sub1
  (lambda (n)
    (- n 1)))

(define triangular
  (lambda (n)
    (cond
      ((zero? n) 0)
      (else (+ n (triangular (sub1 n)))))))

(define vertices
  (lambda (n)
    (sub1 (triangular 1))))