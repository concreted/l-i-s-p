(definitial t #t)
(definitial f #f)
(definitial nil '())

(definitial foo)
(definitial bar)
(definitial fib)
(definitial fact)

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive set-cdr! set-cdr! 2)

(defprimitive + + 2)
(defprimitive * * 2)
(defprimitive - - 2)
(defprimitive / / 2)

(defprimitive eq? eq? 2)
(defprimitive < < 2)
(defprimitive > > 2)

(defprimitive list list -1)
(defprimitive apply (lambda (f vals) (f vals)) 2)
