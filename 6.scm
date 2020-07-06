(load "1.scm")

; assume only + and * 
(define numbered?
  (lambda (aexp)
    ;(display aexp) (newline)
    (cond 
      ((atom? aexp) (number? aexp))
      ((and (atom? (car aexp)) (null? (cdr aexp))) (number? (car aexp)))
      ((eq? (quote +) (car (cdr aexp))) (and (numbered? (car aexp)) (numbered? (cdr (cdr aexp)))))
      ((eq? (quote *) (car (cdr aexp))) (and (numbered? (car aexp)) (numbered? (cdr (cdr aexp))))))))

(define (operator aexp) (car (cdr aexp)))

(define (1st-sub-exp aexp) (car aexp))

(define (2nd-sub-exp aexp) (car (cdr (cdr aexp))))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (quote +) (operator aexp)) (+ (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp))))
      ((eq? (quote *) (operator aexp)) (* (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp)))))))

(value '(5 * (3 + 4)))