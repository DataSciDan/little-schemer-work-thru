#lang racket
(define atom?
  (lambda (s)
    (not (or (pair? s) (null? s)))))

(define mynumbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((atom? (car aexp)) (and (numbered? (car aexp))
                               (or
                                                     (eq? (car (cdr aexp)) '+)
                                                     (eq? (car (cdr aexp)) '*)
                                                     (eq? (car (cdr aexp)) '^))
                               (numbered? (car(cdr (cdr aexp)))))))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp)) ((car (cdr (cdr aexp)))))))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '*) (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '^) (expt (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

(define value2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car nexp) '+) (+ (value2 (car (cdr nexp))) (value2 (car (cdr (cdr nexp))))))
      ((eq? (car nexp) '*) (* (value2 (car (cdr nexp))) (value2 (car (cdr (cdr nexp))))))
      ((eq? (car nexp) '^) (expt (value2 (car (cdr nexp))) (value2 (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (lambda (nexp)
    (car (cdr nexp))))

(define 2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(define operator
  (lambda (nexp)
    (car nexp)))

(define value3
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+) (+ (value3 (1st-sub-exp nexp)) (value3 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '*) (* (value3 (1st-sub-exp nexp)) (value3 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '^) (expt (value3 (1st-sub-exp nexp)) (value3 (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1?
  (lambda (n)
    (cdr n)))

(define plas
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (plas n (zub1? m)))))))