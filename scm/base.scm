;; base.scm

;; primitive token for compiler
(define ysc-begin        '$begin)
(define ysc-if           '$if)
(define ysc-lambda       '$lambda)
(define ysc-get          '$get)
(define ysc-set!         '$set!)
(define ysc-apply        '$apply)
(define ysc-apply-tl     '$apply-tl)
(define ysc-inline-apply '$apply-inline)
(define ysc-apply-cc     '$apply-cc)
(define ysc-with-rule    '$with-rule)
(define ysc-exit         '$exit)
(define ysc-quote        'quote)

;; it is not a primitive token
(define ysc-apply-proc 'apply-proc)

;; to implement or not?
(define ysc-load       '$load)

(define meta-type-ruledef 'ruledef)
(define meta-type-binding 'binding)
(define meta-type-typedef 'typedef)
(define meta-type-error   'error)

;; base.scm ends here.
