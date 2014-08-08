; Initialization
; =========================================================================

(use-syntax (ice-9 syncase)) ;required for define-syntax


; Helper functions
; =========================================================================

(define (println msg)
  (display msg)
  (newline))

; Helper functions for evaluate
(define (atom? e)
  (not (pair? e)))

(define (wrong msg e) 
  (display msg)
  (display ": ")
  (display e)
  (display "\n"))

