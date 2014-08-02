(define (map fn l)
  (if (pair? l)
      (cons (fn (car l)) (map fn (cdr l)))
      '() ) )

(let ((l '("a" "b" "c")))
  (display (map (lambda (x) (list-ref l x))
       '(2 1 0) ) ) )
(newline)

#! 
Expected result is (c b a).

In a dynamic Lisp, result is (0 0 0). 

Why? Since (map) uses variable l, the argument '(2 1 0) 
passed to (map) is captured by l in the (lambda) call
instead of '("a" "b" "c"). 

The map function call then looks like this:

(map (lambda (x) (list-ref l x)) '(2 1 0))
(map (lambda (x) (list-ref '(2 1 0) x)) '(2 1 0))

(map) keeps changing the value of l at each level of recursion, 
due to the (map fn (cdr l)) step.
So the value of l in (lambda) keeps changing also.

  At first level it is:
  (lambda (x) (list-ref '(2 1 0) x))

  Second level:
  (lambda (x) (list-ref '(1 0) x))

  Third level:
  (lambda (x) (list-ref '(0) x))
    
The (map) call expands to:
(cons (fn (2)) (map fn (cdr l)))
(cons (0) (cons (fn (1)) (map fn (cdr l))))
(cons (0) (cons (0) (cons (fn (0)) (map fn (cdr l)))))
(cons (0) (cons (0) (cons (0) '())))
(0 0 0)

!#


