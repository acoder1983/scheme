(load "C:\\Work\\Scheme\\Src\\Simply\\simply.scm")
(define (prime? n)
  (if (= n (smallest-divisor n)) #t
      #f))

(define (smallest-divisor n)
(define (next a)
  (if (= a 2) 3
      (+ a 2)))
  (define (sd-iter n a)
    (cond ((> (* a a) n) n)
          ((= 0 (remainder n a)) a)
          (else (sd-iter n (next a)))))
  (sd-iter n 2))

(define (gen-pi n)
  (define (gen-iter a b p n)
    (if (= n 0) (* 4 p)
        (gen-iter (+ b 1) (+ a 1) (* p (/ a b)) (- n 1))))
  (gen-iter 2.0 3.0 1 n))

(define (product-recur term a next b)
  (if (> a b) 1
      (* (term a) (product-recur term (next a) next b))))

(define (product-iter term a next b)
  (define (prod-iter term a next b p)
    (if (> a b) p
        (prod-iter term (next a) next b (* (term a) p))))
  (prod-iter term a next b 1))

(define (factorial-prod-recur n)
  (product-recur (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (factorial-prod-iter n)
  (product-iter (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (gen-pi-prod n)
  (define (square x) (* x x))
  (/ (square (product-iter (lambda (x) x) 2.0 (lambda (x) (+ x 2)) (* 2 n)))
     (square (product-iter (lambda (x) x) 3.0 (lambda (x) (+ x 2)) (- (* 2 n) 1)))
     n))