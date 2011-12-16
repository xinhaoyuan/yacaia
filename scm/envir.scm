;; envir.scm

(define (make-envir init-levels)

  (letrec ((levels init-levels)
           
           (add-ruledef!
            (lambda (name meta)
              (let ((level (car levels)))
                (and (eq? (car level) meta-type-ruledef)
                     (set-cdr! level
                               (cons (cons name meta) (cdr level)))
                     ))
              ))
           
           (push-ruledef-level
            (lambda (level)
              (let recur ((result '())
                          (current level))
                
                (if (eq? current '())
                    (make-envir (cons (cons meta-type-ruledef result)
                                      levels))
                    (recur (cons (car current) result) (cdr current)))
                )))

           (add-binding!
            (lambda (name meta)
              (let ((level (car levels)))
                (and (eq? (car level) meta-type-binding)
                     (set-cdr! level
                               (cons (cons name meta) (cdr level)))
                     ))
              ))
           
           (push-binding-level
            (lambda (level)
              (let recur ((result '())
                          (current level))
                
                (if (eq? current '())
                    (make-envir (cons (cons meta-type-binding result)
                                      levels))
                    (recur (cons (car current) result) (cdr current)))
                )))

            (add-typedef!
            (lambda (name meta)
              (let ((level (car levels)))
                (and (eq? (car level) meta-type-typedef)
                     (set-cdr! level
                               (cons (cons name meta) (cdr level)))
                     ))
              ))
           
           (push-type-level
            (lambda (level)
              (let recur ((result '())
                          (current level))
                
                (if (eq? current '())
                    (make-envir (cons (cons meta-type-typedef result)
                                      levels))
                    (recur (cons (car current) result) (cdr current)))
                )))
           )
    
    
    (lambda (op . args)
      (cond

       ((eq? op 'levels)
        levels)

       ((eq? op 'add-ruledef!)
        (apply add-ruledef! args))

       ((eq? op 'push-ruledef-level)
        (apply push-ruledef-level args))

       ((eq? op 'add-binding!)
        (apply add-binding! args))
       
       ((eq? op 'push-binding-level)
        (apply push-binding-level args))

       ((eq? op 'add-typedef!)
        (apply add-typedef! args))
       
       ((eq? op 'push-type-level)
        (apply push-typedef-level args))

       ;; for debugging

       ((eq? op 'debug-dump)
        (display "bindings:   ") (display levels) (newline)
        )
       
       (else
        (display "envir: unknown op ") (display op) (newline))
       
       )
      
      )))

(define (empty-envir) (make-envir '()))
(define (level-lookup level sym)
  (assq sym (cdr level)))

(define (envir-lookup-ruledef envir sym)
  (let lookup-recur ((current (envir 'levels)))
    (if (eq? current '()) '()
        (let ((result (and (eq? (car (car current)) meta-type-ruledef)
                           (level-lookup (car current) sym))))
          (if (pair? result)
              (cons meta-type-ruledef result)
              (lookup-recur (cdr current)))))))

(define (envir-lookup-binding envir sym)
  (let lookup-recur ((current (envir 'levels)))
    (if (eq? current '()) '()
        (let ((result (and (eq? (car (car current)) meta-type-binding)
                           (level-lookup (car current) sym))))
          (if (pair? result)
              (cons meta-type-binding result)
              (lookup-recur (cdr current)))))))

(define (envir-lookup-typedef envir sym)
  (let lookup-recur ((current (envir 'levels)))
    (if (eq? current '()) '()
        (let ((result (and (eq? (car (car current)) meta-type-typedef)
                           (level-lookup (car current) sym))))
          (if (pair? result)
              (cons meta-type-typedef result)
              (lookup-recur (cdr current)))))))

(define (envir-lookup envir sym)
  (let lookup-recur ((current (envir 'levels)))
    (if (eq? current '()) '()
        (let ((result (level-lookup (car current) sym)))
          (if (pair? result)
              (cons (car (car current)) result)
              (lookup-recur (cdr current)))))))

(define system-envir ((empty-envir) 'push-ruledef-level '()))

(define (make-binding-meta-data level offset typedef) (vector level offset typedef))
(define (binding-level   meta-data) (vector-ref meta-data 0))
(define (binding-offset  meta-data) (vector-ref meta-data 1))
(define (binding-typedef meta-data) (vector-ref meta-data 2))

;; envir.scm ends here.
