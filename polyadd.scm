(load "accumulate.scm")

(define (polyadd x seq)
  (define (padd i x seq)
    (cond ((null? seq) i)
          (else (padd (+ (* i x) (car seq)) x (cdr seq)))))
  (padd 0 x seq))

(define (polyadd2 x seq)
  (accumulate (lambda (a b) 
                (+ (* a x) b)) 
              0 
              seq))

(define (test-polyadd)
  (and (= (polyadd 2 (list 3 2 1)) 17)
       (= (polyadd2 2 (list 3 2 1)) 17))) ;3*x*x+2*x+1