; REPL
; =========================================================================

; Command line arguments
(define trace? #f)
(define test? #f)

(define (handle-args args)
  (if (pair? args)
      (begin 
	(if (string=? (car args) "-t")
	    (begin
	      (println "Trace on.")
	      (set! trace? #t) ) )
	(if (string=? (car args) "-test")
	    (println "Test!") )
	(handle-args (cdr args))) ) )

(define (chapter1-scheme)
  (define (toplevel)
    ;(display env.global)
    ;(newline)
    (display "l-i-s-p> ")
    (display (evaluate (read) env.global))
    (newline)
    (toplevel))
  (toplevel) )

(handle-args (cdr (command-line)))
(chapter1-scheme)

