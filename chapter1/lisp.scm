; evaluate function takes program expression (e) and environment (env) as input. 
; Environment is a data structure associating variables and values. 
(define (evaluate e env) 
  (if (atom? e)                             ; (atom? e) == (not (pair? e))
      (cond ((symbol? e) (lookup e env))
	    ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
	     e)
	    (else (wrong "Cannot evaluate" e)) )
      (case (car e)
	((quote)  (cadr e))
	((if)     (if (evaluate (cadr e) env)
		      (evaluate (caddr e) env)
		      (evaluate (cadddr e) env) ))
	((begin)  (eprogn (cdr e) env))
	((set!)   (update! (cadr e) env (evaluate (caddr e) env)))
	((lambda) (make-function (cadr e) (cddr e) env))
	(else     (invoke (evaluate (car e) env)
			  (evlis (cdr e) env) )) ) ) )   

