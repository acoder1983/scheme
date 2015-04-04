(define (fixed-point f a)
  (cond ((< (abs (- a (f a))) 0.0001) a)
        ((> (abs (- a (f a))) 100000000000000000000000000000000) '(no fixed point))
        (else (fixed-point f (f a)))))

(define (sqrt-fp x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0))

(define (infi-conti-frac n d k)
  (if (= k 0) 0.0
      (/ n (+ d (infi-conti-frac n d (- k 1))))))

(define (conti-frac n d k)
  (define (conti-frac n d k f)
    (if (= k 0) f
        (conti-frac n d (- k 1) (/ n (+ d f)))))
  (conti-frac n d k 0.0))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define (repeat f n x)
  (if (= n 1) (f x)
      (repeat f (- n 1) (f x))))

(define (repeated f n)
  (lambda (x) (if (= n 1) (f x)
                  ((repeated f (- n 1)) (f x)))))