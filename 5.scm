(load "1.scm")
(load "4.scm")

(define rember*
  (lambda (a l) 
    (cond 
      ((null? l) '())
      ((atom? (car l)) 
        (cond 
          ((eq? a (car l)) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))
; (rember* 'a '((a b) a ((d (a))) (c b a)))
; Value: ((b) ((d ())) (c b))

(define insertR*
  (lambda (new old l)
    (cond 
      ((null? l) '())
      ((atom? (car l)) 
        (cond
          ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
          (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))
; (insertR* 'new* 'a '((a b) a ((d (a))) (c b a)))

(define occur*
  (lambda (a l)
    (cond 
      ((null? l) 0)
      ((atom? (car l))
        (cond
          ((eq? a (car l)) (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))
; (occur* 'a '((a b) a ((d (a))) (c b a)))

(define subst*
  (lambda (new old l)
    (cond 
      ((null? l) '())
      ((atom? (car l)) 
        (cond
          ((eq? old (car l)) (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))
; (subst* 'new 'a '((a b) a ((d (a))) (c b a)))

(define insertL*
  (lambda (new old l)
    (cond 
      ((null? l) '())
      ((atom? (car l)) 
        (cond
          ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
          (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))
; (insertL* 'new* 'a '((a b) a ((d (a))) (c b a)))

(define member*
  (lambda (a l)
    (cond 
      ((null? l) #f)
      ((atom? (car l))
        (cond
          ((eq? a (car l)) #t)
          (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))
; (occur* 'a '((a b) a ((d (a))) (c b a)))

(define leftmost
  (lambda (l)
    (cond 
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2) 
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2))) 
        (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1) (atom? (car l2)))) #f)
      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond 
      ((and (atom? s1) (atom? s2)) (eq? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

; SIMPLIFY eqlist!!!
; (define eqlist?
;   (lambda (l1 l2) 
;     (cond
;       ((and (null? l1) (null? l2)) #t)
;       ((or (null? l1) (null? l2)) #f)
;       (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))