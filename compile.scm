;; compile.scm

(define (compile cps-exp)
  (letrec ((scan (lambda (exp)
                   (if (pair? exp)
                       (let recur ((cur (cdr exp))
                                   (result 0))
                         (if (pair? cur)
                             (recur (cdr cur)
                                    (+ result (scan (car cur))))
                             (+ result (if (eq? (car exp) ysc-lambda) 1 0))
                             ))
                       0
                       )
                   ))
           
           (lambda-vec (make-vector (+ 1 (scan cps-exp))))
           (reader (make-plist-reader cps-exp))
           (writer '())

           (lcount 1)
           (lid-stack (list 0))
           )

    (let init-recur ((count (vector-length lambda-vec)))
      (if (> count 0)
          (begin
            (vector-set! lambda-vec (- count 1) (make-plist-writer))
            (init-recur (- count 1)))))

    (set! writer (vector-ref lambda-vec 0))

    (let recur ((exp cps-exp))
      (cond
       
       ((eq? (car exp) ysc-lambda)
        (apply (lambda (argc body)

                 (let ((id lcount))
                   (set! lid-stack (cons id lid-stack))
                   (set! lcount (+ 1 lcount))
                   (set! writer (vector-ref lambda-vec id))
                   
                   (recur body)

                   (set! lid-stack (cdr lid-stack))
                   (set! writer (vector-ref lambda-vec (car lid-stack)))
                   (list ysc-lambda id)
                   ))
               
               (cdr exp)))

       ((eq? (car exp) ysc-set-cps)
        (apply (lambda (cont ref value)

                 (set! cont (recur cont))
                 (writer 'push-level!)
                 (writer 'write! ysc-set-cps)
                 (writer 'write! cont)
                 (writer 'write! ref)
                 (writer 'write! value)

                 ) (cdr exp)))

       ((eq? (car exp) ysc-apply)
        (begin
          (writer 'push-level!)
          (writer 'write! ysc-apply)

          (let inner-recur ((cur (cdr exp)))

            (if (pair? cur)
                (begin 
                  (writer 'write! (recur (car cur)))
                  (inner-recur (cdr cur)))))
          ))

       ((eq? (car exp) ysc-inline-apply)
        (begin
          (writer 'push-level!)
          (writer 'write! ysc-inline-apply)
          
          (let inner-recur ((cur (cdr exp)))
            
            (if (pair? cur)
                (begin 
                  (writer 'write! (recur (car cur)))
                  (inner-recur (cdr cur)))))
          ))

       ((eq? (car exp) ysc-if)
        (apply (lambda (if-cond if-ture if-false)

                 (writer 'push-level!)
                 (writer 'write! (recur if-cond))
                 (writer 'write! (recur if-true))
                 (writer 'write! (recur if-false))

                 ) (cdr exp)))

       ((eq? (car exp) 'exit) (list ysc-lambda -1))

       (else exp)
       )
      )

    (let after-recur ((count (vector-length lambda-vec)))
      (if (> count 0)
          (begin
            (vector-set! lambda-vec (- count 1) (car ((vector-ref lambda-vec (- count 1)) 'finish!)))
            (after-recur (- count 1)))))

    lambda-vec
  ))

;; compile.scm ends here.
