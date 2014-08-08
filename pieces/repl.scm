; REPL
; =========================================================================

(define (repl)
  (define (toplevel)
    (display "l-i-s-p> ")
    (display (evaluate (read) env.global))
    (newline)
    (toplevel))
  (toplevel) )



