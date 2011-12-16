;; type.scm

(define type-void   'void)
(define type-atomic 'atomic)
(define type-object 'object)

;; XXX
(define (type-lambda-arg type arg-idx) type-object)
(define (type-lambda-ret type) type-object)

;; type.scm ends here.
