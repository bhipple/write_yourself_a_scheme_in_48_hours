; Scheme standard library beginnings

(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))
