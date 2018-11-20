#lang racket
(define atom?
  (lambda (s)
    (not (or (null? s) (pair? s)))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define member?
  (lambda (s l)
    (cond
      ((null? l) #f)
      (else (or (equal? (car l) s) (member? s (cdr l)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(define multirember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (multirember s (cdr l)))
      (else (cons (car l) (multirember s (cdr l)))))))

(define mymakeset2
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (cons (car lat) (mymakeset2 (multirember (car lat) (cdr lat)))))
      (else (cons (car lat) (mymakeset2 (cdr lat)))))))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))

; agnostic as to the implementation of sub-function member?
(define mysubset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((null? set2) #f)
      (else (cond
              ((member? (car set1) set2) (mysubset? (cdr set1) set2))
              (else #f))))))

; incorporates awareness of the implementation of sub-function member?
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

(define subset2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset2? (cdr set1) set2))))))

(define subset3?
  (lambda (set1 set2)
    (or (null? set1) (and (member? (car set1) set2) (subset3? (cdr set1) set2)))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

; agnostic as to the implementation of sub-function member?
(define myintersect?
  (lambda (set1 set2)
    (cond
      ((or (null? set1) (null? set2)) #f)
      ((member? (car set1) set2) #t)
      (else (intersect? (cdr set1) set2)))))

; incorporates awareness of the implementation of sub-function member?
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (intersect? (cdr set1) set2)))))

(define intersect2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect2? (cdr set1) set2))))))

(define myintersection
  (lambda (set1 set2)
    (cond
      ((or (null? set1) (null? set2)) '())
      (else (cond
              ((member? (car set1) set2) (cons (car set1) (myintersection (cdr set1) set2)))
              (else (myintersection (cdr set1) set2)))))))

(define intersection
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersection (cdr set1) set2)))
      (else (intersection (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersection (car l-set) (intersectall (cdr l-set)))))))

; I don't like calling it a-pair? as I don't want to get it confused with pair?
(define my-a-double?
  (lambda (x)
    (cond
      ((not (pair? x)) #f)
      (else (cond
              ((null? (cdr x)) #f)
              (else (null? (cdr (cdr x)))))))))

(define a-double?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x) #f))
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define my-other-a-double?
  (lambda (x)
    (cond
      ((not (pair? x)) #f)
      ((not (pair? (cdr x))) #f)
      (else (null? (cdr (cdr x)))))))

(define first
  (lambda (d)
    (car d)))

(define second
  (lambda (d)
    (car (cdr d))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (t)
    (car (cdr (cdr t)))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

; rel is short for relation, fun for cartesian function
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))))))

(define revdouble
  (lambda (d)
    (build (second d) (first d))))

(define revrel2
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revdouble (car rel)) (revrel (cdr rel)))))))

(define seconds
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (second (car rel)) (seconds (cdr rel)))))))

; should take not simply rel's but functions in particular
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

; fullfun? could also be called one-to-one? cuz muh linear algebra
; here's another way to define it
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
