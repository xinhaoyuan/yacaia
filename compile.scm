;; compile.scm

(define system-macro-head?
  (make-fixed-set
   (list ysc-begin
         ysc-if
         ysc-lambda
         ysc-set!
         ysc-apply
         ysc-apply-cc
         ysc-inline-apply
         ysc-with-macro
         ysc-quote)))

;; it is not a primitive token
(define ysc-apply-proc 'apply-proc)

(define compile-system-macro
  (lambda (log
           context
           tail? 
           level-size			; level of binding
           head-symbol
           exp				    ; without context
           )

    (cond

     ((eq? head-symbol ysc-quote)
      (compile-constant (purge-exp context exp))
      )
     
     ((eq? head-symbol ysc-set!)
      (apply
       (lambda (variable value)

         (let ((result
                (call-with-context
                 (lambda (context name)
                   (set! variable name)
                   (context-lookup-variable context variable))
                 context variable)))

           (list ysc-set! (if result
                              (make-variable-access level-size (cdr result))
                              variable)
                 (compile-eval log context #f level-size value))
           ))
       exp)
      )

     ((eq? head-symbol ysc-begin)
      (if (eq? exp '())
          (log 'complain-error "the Begin expression cannot be empty")
      	  (let eval-recur ((result (deque))
                           (current exp))
            
            (if (eq? (cdr current) '())
                ;; tail expression
                (begin 
                  (result 'push-head! ysc-begin)
                  (result 'push-tail! (compile-eval log context tail? level-size (car current)))
                  (result 'plist))
                
                (eval-recur (begin (result 'push-tail! (compile-eval log context #f level-size (car current)))
                                   result)
                            (cdr current)))
            ))
      )

     ((eq? head-symbol ysc-if)
      (apply
       (lambda (condition if-true if-false)

         (let ((comp-ift
                (compile-eval log context tail? level-size if-true))
               (comp-iff
                (compile-eval log context tail? level-size if-false))
               )

           (list ysc-if
                 (compile-eval log context #f level-size condition)
                 comp-ift
                 comp-iff
                 )
           )
         
         ) exp))

     ((eq? head-symbol ysc-lambda)
      (let ((args-list
             (call-with-context
              (lambda (context list)
                (purge-plist context list))
              context (car exp)))
            (args-count 0)
            (codes-list (cdr exp))
            (level-size (+ level-size 1)))

        (context
         (lambda (mx-mode? envir)

           ;; (display "???") (display args-list) (newline)
           (let count-recur ((before '())
                             (current args-list))
             (if (pair? current)
                 (if (symbol? (car current))
                     (begin
                       (set! args-count (+ args-count 1))
                       (count-recur current (cdr current))
                       )
                     (log 'complain-error "There are non-symbol in arglist"))
                 
                 (if (symbol? current)
                     (begin
                       (if (eq? before '())
                           (set! args-list (list current))
                           (set-cdr! before (cons current '())))
                       (set! args-count (- (+ args-count 1)))
                       )
                     (if (not (eq? current '()))
                         (log 'complain-error "There are non-symbol in arglist")))
                 ))
           
           (list
            ysc-lambda
            args-count
            (compile-system-macro log
                                  
                                  (make-context
                                   mx-mode?
                                   (envir 'push-variable-level
                                          level-size
                                          args-list))
                                  #t
                                  level-size
                                  ysc-begin
                                  codes-list
                                  ))

           ))
        ))

     ((eq? head-symbol ysc-apply)
      (let ((proc (car exp))
            (args (cdr exp)))

        (if (not (pair? args))
            (log 'complain-error "Cannot apply since incorrect format")
            (let recur ((result (deque))
                        (current args))

              (if (pair? (cdr current))

                  (recur
                   (begin
                     (result 'push-tail!
                             (compile-eval log context #f level-size
                                           (car current)))
                     result)
                   (cdr current))

                  (begin
                    (result 'push-head! (compile-eval log context #f level-size proc) ysc-apply)
                    (result 'push-tail! (if
                                         (eq? (purge context (car current)) '())
                                         '()
                                         (compile-eval log context #f level-size (car current)))
                            )
                    (result 'plist))
                  )))
        ))

     ((eq? head-symbol ysc-apply-cc)
      (if (and (pair? exp)
               (eq? (cdr exp) '())
               )

          (list ysc-apply-cc
                (compile-eval log context #f level-size (car exp))
                )
          
          (log 'complain-error
               "Syntax error with @apply-cc")
          )
      )

     ((eq? head-symbol ysc-inline-apply)
      (let ((head (purge context (car exp)))
            (args (cdr exp)))

        (let recur ((result (deque))
                    (current args)
                    (length 0))

          (if (pair? current)

              (recur
               (begin
                 (result 'push-tail!
                         (compile-eval log context #f level-size
                                       (car current)))
                 result)
               (cdr current)
               (+ length 1))
              
              (begin
                (result 'push-head! length head ysc-inline-apply)
                (result 'plist)
                ))
          ))
      )

     ((eq? head-symbol ysc-with-macro)

      (context
       (lambda (mx-mode? envir)
         ;; set a new macro binding level
         (set! envir (envir 'push-macro-level '()))
         
         (let recur ((current exp))
           (if (pair? (cdr current))
               ;; compile the expression converted
               (compile-eval
                log
                (make-context mx-mode? envir)
                tail?
                level-size
                (car current))

               (begin
                 (apply
                  call-with-context
                  (lambda (context macro-name macro-labels . rules-list)
                    (set! macro-name (purge context macro-name))
                    (if (not (symbol? macro-name))
                        (log 'complain-error "Syntax error with macro name"))
                    
                    (set! macro-labels
                          (make-fixed-set
                           (call-with-context
                            (lambda (context list)
                              (purge-list context list))
                            context macro-labels)))
                    
                    (let ((result (deque)))
                      
                      (let recur ((current rules-list))
                        
                        (if (eq? current '())
                            (envir 'add-macro!
                                   (cons macro-name
                                         (result 'plist)))
                            (if (pair? (cdr current))
                                (log 'complain-error "Syntax error with rules list")
                                
                                (begin
                                  (result 'push-tail!
                                          ;; 					  (cons
                                          ;; 					   envir
                                          (compile-hygienic-rule
                                           context
                                           (lambda (error) (log 'complain-error error))
                                           macro-labels
                                           (car current)
                                           (car (cdr current)))
                                          ;; 					   )
                                          )
                                  
                                  (recur (cdr (cdr current))))
                                )))
                      ))
                  context (car current))
                 (recur (cdr current)))
               ))
         )))
     )
    ))

;; find the first matchable macro and apply it
(define apply-macro
  (lambda (macro context exp)
    (let apply-recur ((current macro))

      (let ((label-size  (car (car current)))
            (input-rule  (car (cdr (car current))))
            (output-rule (cdr (cdr (car current)))))
        
        (or
         (let ((label-vec
                (read-hygienic-rule context exp label-size input-rule)))
           
           (and label-vec
                (write-hygienic-rule label-vec
                                     label-size
                                     output-rule)
                ))
         (if (eq? (cdr current) '()) #f
             (apply-recur (cdr current)))))
      )
    ))

(define compile-eval
  (lambda (log context tail? level-size exp)

    (call-with-context
     (lambda (context exp)

       ;; (context
       ;;  (lambda (mx-mode? envir)
       ;;    (display "now : ") (display mx-mode?) (display " --- ")
       ;;    (display (purge-exp context exp)) (newline)))
       
       (cond

        ((symbol? exp)
         (let ((result (context-lookup-variable context exp)))
           (if result
               (compile-variable (make-variable-access level-size (cdr result)))
               (begin
                 (set! result (vm-binding-lookup exp))
                 (if result
                     (compile-vm-binding result)
                     (log 'complain-error "Binding cannot be found in current environment"))
                 )
               )))

        ((not (pair? exp))
         (context
          (lambda (mx-mode? envir)
            (if mx-mode?
                (compile-constant (list (purge context exp)))
                (compile-constant (list exp))))
          ))

        (else

         (call-with-context
          (lambda (head-context
                   head-exp)

            (let ((head (if (system-macro-head? head-exp)
                            head-exp
                            (context-lookup head-context head-exp))))

              (cond
               
               ((symbol? head)            ; system macro
                (compile-system-macro log context tail?
                                      level-size head (cdr exp)
                                      ))
               
               ((or (not (pair? head))
                    (eq? (car head) 'variable))
                (let ((result
                       (context-lookup-macro context ysc-apply-proc)))
                  (if result
                      (begin
                        (set! result (apply-macro (cdr result)
                                                  context
                                                  exp))
                        (if result
                            (compile-eval log (context
                                               (lambda (mx-mode? envir)
                                                 (make-context #t envir)))
                                          tail?
                                          level-size result)
                            (log 'complain-error
                                 "Cannot apply apply-proc macro!")
                            ))
                      (log 'complain-error
                           "The ``apply-proc'' macro not exist in current context"))
                  ))
               
               ((eq? (car head) 'macro)
                
                (let ((result (apply-macro (cdr head) 
                                           context (cdr exp))))
                  (if result
                      (begin
                        (compile-eval log (context
                                           (lambda (mx-mode? envir)
                                             (make-context #t envir)))
                                      tail?
                                      level-size result)
                        )
                      (log 'complain-error "Cannot apply the macro"))))
               
               ;; ((eq? (car head) 'variable)
               ;;  (let ((result (cdr head)))
               ;;    (if result
               ;;      (compile-variable (make-variable-access level-size result))
               ;;      (compile-vm-binding (vm-binding-lookup result)))))
               ))
            ) context (car exp))
         
         ))
       
       )

     context exp)))

(define compile-with-system-envir
  (lambda (exp)

    (call-with-current-continuation
     (lambda (return)
       
       (compile-eval (lambda (op . args)
                       
                       (cond
                        ((eq? op 'complain-error)
                         (display "Error while compiling : ")
                         (display (car args))
                         (newline)
                         
                         (return '())))
                       )
                     (make-context #f system-envir)
                     #t
                     0
                     exp)
       ))

    ))

(add-macro! system-envir

            ysc-apply-proc

            '(proc args)

            '(proc args ...)
            (list ysc-apply 'proc '(@expand args) '())

            )

;; compile.scm ends here.
