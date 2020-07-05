(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond 
        ((eq? a (car lat)) (cdr lat))
        (else (cons (car lat) (rember a (cdr lat)))))))))

; ??? why cant simplify in this way?
(define rember2
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember2 a (cdr lat)))))))

(define first
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (first (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      (else (cond
        ((eq? old (car lat) (cons old (cons new (cdr lat))))
        (else (cons (car lat) (insertR new old (cdr lat))))))))))

; insertL similarly

(define subst
  (lambda (new old lat)
    (cond
      ((null? l) '())
      (else (cond
        ((eq? old (car lat)) (cons new (cdr lat)))
        (else (cons (car lat) (subst new old (cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else (cond
        ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
        else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))))

; remove all occurence
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond 
        ((eq? a (car lat)) (multirember a (cdr lat)))
        (else (cons (car lat) (multirember a (cdr lat)))))))))

; multiinsertR (similar)

; multiinsertL
(define multiinsertL
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      (else (cond
        ((eq? old (car lat) (cons new (cons old (multiinsertL new old (cdr lat)))))
        (else (cons (car lat) (multiinsertL new old (cdr lat))))))))))

(first '(((a a) b) (c d) (e f)))


