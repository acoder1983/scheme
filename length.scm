(define (list-len-recur lst)
  (if (null? lst) 0
      (+ 1 (list-len-recur (cdr lst)))))

(define (list-len-iter lst)
  (define (lst-len-iter lst len)
    (if (null? lst) len
        (lst-len-iter (cdr lst) (+ len 1))))
  (lst-len-iter lst 0))

(define (append-recur a b)
  (if (null? a) b
      (cons (car a) (append (cdr a) b))))

(define (append-recur2 a b)
  (if (null? b) a
      (append-recur2 (cons a (car b)) (cdr b))))