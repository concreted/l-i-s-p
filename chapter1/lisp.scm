; evaluate: takes program expression (e) and environment (env) as input. 
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

; eprogn: Evaluate expressions (exps) with environment (env) sequentially in order.
; Only executes if (exps) is a pair? 
(define (eprogn exps env) 
  (if (pair? exps)
      (if (pair? (cdr exps))
	  (begin (evaluate (car exps) env)
		 (eprogn (cdr exps) env) )
	  (evaluate (car exps) env) )
      empty-begin) )

(define empty-begin 813)

; evlis: Evaluate list of expressions (exps) and returns result values in a list.
; Evaluates right-to-left.
(define (evlis exps env)
  (if (pair? exps)
      (cons (evaluate (car exps) env)
	    (evlis (cdr exps) env) )
      '() ) )

; lookup: Lookup variable (id) in the environment association list (env).
(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
	  (cdar env)
	  (lookup id (cdr env)) )
      (wrong "No such binding" id) ))

; update!: Sets variable (id) in environment (env) to a value (value).
(define (update! id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
	  (begin (set-cdr! (car env) value)
		 value)
	  (update! id (cdr env) value) )
      (wrong "No such binding" id) ))

(define env.init '())

; extend: Adds list of variables (variables) and corresponding 
; values (values) to environment (env)
(define (extend env variables values)
  (cond ((pair? variables)
	 (if (pair? values)
	     (cons (cons (car variables) (car values))
		   (extend env (cdr variables) (cdr values)) )
	     (wrong "Too few values") ) )
	((null? variables)
	 (if (null? values)
	     env
	     (wrong "Too many values") ) )
	((symbol? variables) (cons (cons variables values) env)) ) )

; invoke: Call a function (fn) with arguments (args)
(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (wrong "Not a function" fn) ) )

; make-function: Creates a new environment used during function execution.
; Creates new environment by adding variables bound during function call 
; (variables) to parent environment (env), and executes function body
; (body) in that environment.
(define (make-function variables body env)
  (lambda (values)
    (eprogn body (extend env variables values)) ) )

#!
; evlis - defined with left-to-right evaluation. 
(define (evlis exps env)
  (if (pair? exps)
      (let ((argument1 (evaluate (car exps) env)))
	(cons argument1 (evlis (cdr exps) env)) )
      '() ) )
!#