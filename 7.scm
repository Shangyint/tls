; member? e lat
(load "2.scm")
(load "1.scm")

(define makeset
  (lambda (lat) 
    (cond 
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

; using multirember
; (define makeset
;   (lambda (lat)
;   (cond
;     ((null? lat) (quote ()))
;     (else (cons (car lat)
;       (makeset (multirember (car lat) (cdr lat ))))))))

(define subset?
  (lambda (set1 set2) 
    (cond 
      ((null? set1) #f)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

; bad
; (define eqset?
;   (lambda (set1 set2)
;     (cond
;       ((null? set1) (null? set2))
;       ((member? (car set1) set2) (eqset? (cdr set1) (multirember (car set1) set2)))
;       (else #f))))

(define (eqset? set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))

(define (intersect? set1 set2)
  (cond ((null? set1) #f)
    (else (or (member? (car set1) set2) (intersect? (cdr set1) set2)))))

(define (intersect set1 set2)
  (cond 
    ((null? set1) '())
    ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
    (else (intersect (cdr set1) set2))))

; union is similar
; intersectall
(define (intersectall lset)
  (cond ((null? (cdr lset)) (car lset))
    (else (intersect (car lset) (intersectall (cdr lset))))))

(define (a-pair? l)
  (cond ((atom? l) #f)
    ((null? l) #f)
    ((null? (cdr l)) #f)
    ((null? (cdr (cdr l))) #t)
    (else #f)))

(define (first p)
  (car p))

(define (second p)
  (car (cdr p)))

(define (build f s)
  (cons f (cons s '())))

(define (revert rel)
  (cond ((null? rel) '())
    (else (cons (build (second (car rel)) (first (car rel))) (revert (cdr rel))))))