#lang racket
(define myrember*
  (lambda (a l)
    (cond
      ((null? l) '())
      (else
       (cond
         ((pair? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l)))))))))

(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
              (cond
                ((eq? (car l) a) (rember* a (cdr l)))
                (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

; (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
; (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eq? (car l) a) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define myeqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (cond
         ((and (atom? (car l1)) (atom? (car l2)))
          (cond
            ((eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
            (else #f)))
         ((or (atom? (car l1)) (atom? (car l2))) #f)
         (else (and (eqlist? (car a1) (car a2)) (eqlist? (cdr l1) (cdr l2)))))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? a1) (null? a2)) #t)
      ((or (null? a1) (null? a2)) #f)
      ((and (atom? (car a1) (car a2)) (and (eqan? (car a1) (car a2)) (eqlist? (cdr a1) (cdr a2)))))
      ((or (atom? l1) (atom? l2)) #f)
      (else (and (eqlist? (car a1) (car a2)) (eqlist? (cdr a1) (cdr a2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define equal2?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist2? s1 s2)))))

(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal2? (car l1) (car l2)) (eqlist2? (cdr l1) (cdr l2)))))))


(define rember
  (lambda (s l)
    (cond
      ((null? l) l)
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))