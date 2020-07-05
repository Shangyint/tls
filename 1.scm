; chapter 1

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; car defined for non-empty lists
; first element (atom or S-expression)

; cdr defined for non-empty lists
; the rest elements (always a list)

; cons takes two arguments. Second must be a list
; result a list

; null? true for empty list (quote ()), false for all

; eq? takes two non-numeric atom arguments