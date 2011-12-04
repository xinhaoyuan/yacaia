;; compile-base.scm

(define (make-code-block . code)
  (if (eq? code '()) (deque)
      (apply deque code)))

(define (merge-code-blocks! . blocks)
  (let merge-recur ((result (make-code-block))
		    (current blocks))
    
    (if (eq? current '()) result
	(begin
	  (result 'merge-tail! (car current))
	  (merge-recur result (cdr current))))
    ))

(define externalize-code-block
  (lambda (deque)
    (deque 'push-tail! vm-ret)
    (list->vector (deque 'plist))))

(define code-byte-size
  (lambda (code)
    (code 'size)))

(define make-variable-access
  (lambda (level-size variable)
    (cons (- level-size (car variable))
	  (cdr variable))))

(define (vm-binding-lookup symbol)
  symbol)

(define (compile-constant exp)
  (cons ysc-quote exp))

(define (compile-vm-binding binding)
  (list ysc-get binding))

(define (compile-variable variable)
  (list ysc-get (car variable) (cdr variable)))

;; compile-base.scm ends here.
