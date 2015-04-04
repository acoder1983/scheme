(define (fib-recur n)
  (if (< n 3) n
      (+ (fib-recur (- n 1)) (fib-recur (- n 2)))))

(define (fib-iter n)
  (define (fib-iter2 a b n)
    (if (= 0 n) a
        (fib-iter2 (+ a b) a (- n 1))))
  (fib-iter2 2 1 (- n 2)))

(define (f-iter n)
  (define (f-iter2 a b c n)
    (if (= 0 n) a
        (f-iter2 (+ a (* 2 b) (* 3 c)) a b (- n 1))))
  (f-iter2 3 2 1 (- n 3)))

(define (expt-recur a n)
  (if (= n 0) 1
      (* a (expt-recur (- n 1)))))

(define (expt-iter a n)
  (define (expt-iter2 a p n)
    (if (= n 0) p
        (expt-iter2 a (* a p) (- n 1))))
  (expt-iter2 a 1 n))