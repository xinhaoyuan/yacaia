;; type.scm

(define type-void   'void)
(define type-atomic 'atomic)
(define type-object 'object)

(define type-head-typed-lambda 'typed-lambda)
(define type-head-lambda 'lambda)

(define type-head-typed-lambda-cps 'type-lambda-cps)
(define type-head-lambda-cps 'lambda-cps)

(define (type-typed-lambda ret-type args-type-list)
  (cons type-head-typed-lambda
        (cons ret-type args-type-list)))

(define (type-lambda args-count)
  (cons type-head-lambda
        (cons args-count '())))

(define (type-typed-lambda-cps args-type-list)
  (cons type-head-typed-lambda-cps args-type-list))

(define (type-lambda-cps args-count)
  (cons type-head-lambda-cps (cons args-count '())))

;; XXX
(define (type-lambda-arg type arg-idx) type-object)
(define (type-lambda-ret type) type-object)

;; type.scm ends here.
