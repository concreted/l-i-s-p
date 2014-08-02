(load "lisp.scm")

(display (evaluate 1 env.init)) 
(newline)
(display (evaluate (add 1 2) env.init))
(newline)

(set! x 5)