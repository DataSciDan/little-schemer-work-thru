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
      ((or (null? l1) (null? l1)) #f)
      (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define ho-rember-f
  (lambda (test? s l)
    (cond
      ((null? l) '())
      ((test? (car l) s) (cdr l))
      (else (cons (car l) (ho-rember-f test? s (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(define closure-rember-f
  (lambda (test?)
    (lambda (s l)
      (cond
        ((null? l) '())
        ((test? (car l) s) (cdr l))
        (else (cons (car l) ((closure-rember-f test?) s (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons new l))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g seqL))

(define insertR (insert-g seqR))

; another possibility, were we to not define helper functions:
; (define insertL
;   (insert-g
;     (lambda (new old l)
;       (cons new (cons old l)))))

(define seqS
  (lambda (new old l)
    (cons new (cdr l))))

(define subst (insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))

(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x '*) *)
      (else expt))))

(define operator
  (lambda (nexp)
    (car nexp)))

(define 1st-sub-exp
  (lambda (nexp)
    (car (cdr nexp))))

(define 2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(define multrember-eq (multirember-f eq?))

(define eq?-tuna (eq?-c 'tuna))

; multiremberT takes a function like eq?-tuna and a lat and doesn't take an atom and rather than return a function, returns a lat
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))

; col stands for collector or continuation, which is a category of functions
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? (car lat) a) (multirember&co a (cdr lat) (lambda (newlat seen)
                                                       (col newlat
                                                            (cons (car lat) seen)))))
      (else (multirember&co a (cdr lat) (lambda (newlat seen)
                                          (col (cons (car lat) newlat) seen)))))))

; one col you might pass to multirember&co is:
(define a-friend
  (lambda (x y)
    (null? y)))

; in which case, the anonymous function that is passed in recursion when a is tuna and lat is (tuna):
(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons 'tuna seen))))

; whereas the anonymous function that is passed in recursion when a is tuna and lat is (and tuna):
(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat) seen)))