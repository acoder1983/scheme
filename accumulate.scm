(load "list-equal.scm")
(define (accumulate op initial seq)
  (cond ((null? seq) initial)
        (else (accumulate op (op initial (car seq)) (cdr seq)))))

(define (test-acm)
  (and (list-equal? (accumulate + 0 (list 1 2 3)) 6)
       (list-equal? (accumulate max 0 (list 1 3 2)) 3)))