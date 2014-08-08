; Environment definition
; =========================================================================

(define env.init '())
(define empty-begin 813)
(define env.global env.init)

(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (begin (set! env.global (extend env.global '(name) '(void)))
     ;(set! env.global (cons (cons 'name (car env.global)) (cons 'void (cdr env.global))))
	    'name ) ) 
    ((definitial name value)
     (begin (set! env.global (extend env.global '(name) (list value)))
     ;(set! env.global (cons (cons 'name (car env.global)) (cons value (cdr env.global))))
	    'name ) )) )

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name
       (lambda (values)
	 (if (or (= arity -1) (= arity (length values)))
	     (apply value values)
	     (wrong "Incorrect arity"
		    (list 'name values) ) ) ) ) ) ) )