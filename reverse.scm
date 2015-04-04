(define (reverse-recur lst)
  (define (reverse-iter a ret)
    (if (null? a) ret
        (reverse-iter (cdr a) (cons (car a) ret))))
  (reverse-iter lst ()))

(define (last lst)
  (if (null? (cdr lst)) (car lst)
      (last (cdr lst))))

(define (square-list items)
  (define (iter things answer)
    (if (null? things) answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items ()))

(define (square x)
  (* x x))

(define (square-lst items)
  (if (null? items) ()
      (cons (square (car items)) (square-lst (cdr items)))))

(define (count-leaves items)
  (cond ;((null? items) 0)
        ((not (pair? items)) 1)
        (else (+ (count-leaves (car items)) (count-leaves (cdr items))))))

(define (list-len-recur lst)
  (if (null? lst) 0
      (+ 1 (list-len-recur (cdr lst)))))

(define (deep-reverse items)
  