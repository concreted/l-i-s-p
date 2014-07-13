; evaluate function takes program (exp) and environment (env) as input. 
; Environment is a data structure associating variables and values. 
(define (evaluate exp env) 
  (if (atom? exp)                             ; (atom? exp) == (not (pair? exp))
      (cond ((symbol? exp) (lookup exp env))
	    ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
	     e)
	    (else (wrong "Cannot evaluate" e)) )
      ... ) )
   

