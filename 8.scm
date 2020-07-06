(define insert-g
  (lambda (seq) 
    (lambda (new old l)
      (cond 
        ((null? l) '())
        ((eq? old (car l)) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

; tons of CPS stuff