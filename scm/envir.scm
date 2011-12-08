;; envir.scm

(define (make-envir init-binding-levels)

  (letrec ((binding-levels init-binding-levels)
	   (add-macro
            (lambda (macro)
              (let ((level (car binding-levels)))
                (and (eq? (car level) 'macro)
                     (set-cdr! level
                               (cons macro (cdr level)))
                     ))
              ))
           
           (push-variable-level
            (lambda (level-size level)
		 
		 (let recur ((result '())
			     (current-id 0)
			     (current level))
		   
		   (if (eq? current '())
		       (make-envir (cons (cons 'variable result)
                                         binding-levels))
		       (recur (cons
			       (cons (car current)
				     (cons level-size current-id))
			       result)
			      (+ current-id 1)
			      (cdr current)))
		   )))
           (push-macro-level
            (lambda (level)
              (let recur ((result '())
                          (current level))
                
                (if (eq? current '())
                    (make-envir (cons (cons 'macro result)
                                      binding-levels))
                    (recur (cons (car current) result) (cdr current)))
                )))
           )
           
    
    (lambda (op . args)
      (cond

       ((eq? op 'binding-levels)
	binding-levels)

       ((eq? op 'add-macro!)
	(apply add-macro args))
       
       ((eq? op 'push-variable-level)
	(apply push-variable-level args))

       ((eq? op 'push-macro-level)
	(apply push-macro-level args))

       ;; for debugging

       ((eq? op 'debug-dump)
	(display "bindings:   ") (display binding-levels) (newline)
        )
       
       (else
	(display "envir: unknown op.") (display op) (newline))
       
       )
      
      )))

(define empty-envir
  (lambda ()
    (make-envir '())))

(define level-lookup
  (lambda (level sym)
    (let ((result (assq sym (cdr level))))
      (and (pair? result) (cons (car level) (cdr result)))
      )
    ))

(define (envir-lookup-variable envir sym)
  (let lookup-recur ((current (envir 'binding-levels)))
    (if (eq? current '()) #f
        (or (and (eq? (car (car current)) 'variable)
                 (level-lookup (car current) sym))
            (lookup-recur (cdr current))))
    )
  )

(define (envir-lookup-macro envir sym)
  (let lookup-recur ((current (envir 'binding-levels)))
    (if (eq? current '()) #f
        (or (and (eq? (car (car current)) 'macro)
                 (level-lookup (car current) sym))
            (lookup-recur (cdr current))))
    )
  )

(define (envir-lookup envir sym)
  (let lookup-recur ((current (envir 'binding-levels)))
    (if (eq? current '()) #f
        (or (level-lookup (car current) sym)
            (lookup-recur (cdr current))))
    ))

(define system-envir ((empty-envir) 'push-macro-level '()))

;; envir.scm ends here.
