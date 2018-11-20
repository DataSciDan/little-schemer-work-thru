#lang racket
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define addbyincrement
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (addbyincrement n (sub1 m)))))))

(define subbyincrement
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (subbyincrement n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (+ (car tup) (addtup (cdr tup)))))))

(define multiplybyadding
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
       (+ n (multiplybyadding n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define greaterthan
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
       (greaterthan (sub1 n) (sub1 m))))))

(define lessthan
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lessthan (sub1 n) (sub1 m))))))

(define equals
  (lambda (n m)
    (cond
      ((or (> n m) (< n m)) #f)
      (else #t))))

(define myownequals
  (lambda (n m)
    (not (or (> n m) (< n m)))))

(define pow
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (pow n (sub1 m)))))))

(define integerdivision
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (integerdivision (- n m) m))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((number? (car lat)) (no-nums (cdr lat)))
              (else (cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))


(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ((eq? a (car lat)) (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    ((= n 1))))

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))