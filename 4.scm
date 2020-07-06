(define add1 (lambda (a) (+ a 1)))
(define sub1 (lambda (a) (- a 1)))

; o+
(define o+ 
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (o+ (add1 n) (sub1 m))))))

; text o+
(define o+2 
  (lambda (n m)
  (cond
      ((zero? m) n)
      (else (add1 (o+2 n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond ((zero? m) n)
      (else (o- (sub1 n) (sub1 m))))))

(define addtup
  (lambda (tup) 
    (cond ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define o* 
  (lambda (n m) 
    (cond ((zero? m) 0)
      (else (o+ n (o* n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond ((and (null? tup1) (null? tup2)) '())
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define o> 
  (lambda (n m) 
    (cond ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o< 
  (lambda (n m) 
    (cond ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m) 
    (cond ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (o= (sub1 n) (sub1 m))))))

(define exp
  (lambda (n m)
    (cond ((zero? m) 1)
      (else (o* n (exp n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat) 
    (cond ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat) 
    (cond ((zero? (sub1 n)) (cdr lat))
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
      ((and (number? a1) (number? a2)) (= a1 a2 ))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define one? (lambda (x) (zero? (sub1 x))))

