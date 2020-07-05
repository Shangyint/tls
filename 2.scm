; lat? a lat is a list of atoms

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; lat? can only be applied on list
(define lat? 
  (lambda (x) 
    (or 
      (null? x)
      (and (atom? (car x)) (lat? (cdr x))))))

(define lat2?
  (lambda (l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat2? (cdr l )))
    (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat))))))

(lat? '(a (a b)))
(member? 'a '(c b s))