(define (sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b)))) 

(define (sum-iter term a next b)
  (define (sum2 term a next b ret)
    (if (> a b) ret
        (sum2 term (next a) next b (+ (term a) ret))))
  (sum2 term a next b 0))