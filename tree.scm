(load "C:\\Work\\Scheme\\Src\\Simply\\list-equal.scm")
(define x (list (list 1 2) (list 3 4)))

(define (fringe tree)
  (cond ((not (pair? tree)) tree)
        ((null? (cdr tree)) (fringe (car tree)))
        (else (cons (fringe (car tree)) (list (fringe (cdr tree)))))))