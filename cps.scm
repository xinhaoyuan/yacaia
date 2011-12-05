;; cps.scm

(define system-macro-head?
  (make-fixed-set
   (list ysc-begin
         ysc-if
         ysc-lambda
         ysc-set!
         ysc-apply
         ysc-inline-apply
         ysc-apply-tl
         ysc-apply-cc
         ysc-with-macro
         ysc-quote)))

(define cps-system-macro
  (lambda (log
           context
           cont
           level                        ; level of binding
           head-symbol
           exp                          ; without context
           )

    (cond

     ((eq? head-symbol ysc-quote)
      (list ysc-apply (cont level) (list ysc-quote (purge-exp context exp)))
      )

     ((eq? head-symbol ysc-get)
      (let ((result
             (call-with-context
              (lambda (context name)
                (context-lookup-variable context name))
              context exp)
             ))

        (list ysc-apply (cont level)
              (list ysc-get (if result
                                (make-variable-access level (cdr result))
                                exp)))
        ))

     ((eq? head-symbol ysc-set!)
      (apply
       (lambda (variable value)
         
         (let ((variable
                (call-with-context
                 (lambda (context name)
                   (or (context-lookup-variable context name)
                       name))
                 context variable)))

           (cps-eval log context
                     (lambda (cur-level)
                       (list ysc-lambda 1
                             (list ysc-set-cps (cont (+ 1 cur-level))
                                   (if (symbol? variable)
                                       variable
                                       (make-variable-access cur-level (cdr variable)))
                                   (list ysc-get (cons 0 0)))))
                     level value)))
       exp))

     ((eq? head-symbol ysc-begin)
      (if (eq? exp '())
          (log 'complain-error "the Begin expression cannot be empty")
      	  (let eval-recur ((result cont)
                           (current (reverse exp)))
            
            (if (eq? (cdr current) '())
                ;; tail expression
                (cps-eval log context
                          result
                          level (car current))
                (eval-recur
                 (let ((cont result)
                       (exp (car current)))
                   (lambda (cur-level)
                     (list ysc-lambda 1
                           (cps-eval log context
                                     cont
                                     (+ cur-level 1)
                                     exp))))
                 (cdr current)
                 ))
            )))

     ((eq? head-symbol ysc-lambda)
      (let ((args-list
             (call-with-context
              (lambda (context list)
                (purge-plist context list))
              context (car exp)))
            (args-count 0)
            (codes-list (cdr exp)))

        (context
         (lambda (mx-mode? envir)

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
           
           (list ysc-apply (cont level)
                 (list ysc-lambda
                       ;; one for continuation
                       (+ 1 args-count)
                       (cps-system-macro
                        log
                        (make-context
                         mx-mode?
                         (envir 'push-variable-level
                                (+ 1 level)
                                ;; first variable is the cont
                                (cons '() args-list)))
                        ;; get the continuation from arg 0
                        (let ((old (+ 1 level)))
                          (lambda (new)
                            (list ysc-get (cons (- new old) 0))))
                        (+ level 1)
                        ysc-begin
                        codes-list
                        )))
           ))
        ))

     ((eq? head-symbol ysc-if)
      (apply
       (lambda (condition if-true if-false)
         
         (letrec ((inner-cont (lambda (cur-level)
                                (list ysc-get (cons (- cur-level (+ 1 level)) 0))))
                  (cond-cont (lambda (cur-level)
                               (list ysc-lambda 1
                                     (list ysc-if (list ysc-get (cons 0 0))
                                           (cps-eval log context inner-cont (+ 1 cur-level) if-true)
                                           (cps-eval log context inner-cont (+ 1 cur-level) if-false))))))
           (list ysc-apply
                 (list ysc-lambda 1
                       (cps-eval log context cond-cont (+ 1 level) condition))
                 (cont level)))
         ) exp))

     ((or (eq? head-symbol ysc-apply)
          (eq? head-symbol ysc-apply-tl)
          (eq? head-symbol ysc-inline-apply))
      (if (not (pair? exp))
          (log 'complain-error "Cannot apply since incorrect format")
          (let ((args-level-list '()))
            (let scan-recur ((result (lambda (cur-level)
                                       (set! cur-level (+ 1 cur-level))
                                       (set! args-level-list (cons cur-level args-level-list))
                                       (let gen-recur
                                           ((level-list args-level-list)
                                            (result '()))
                                         (if (pair? level-list)
                                             (gen-recur (cdr level-list)
                                                        (cons (list ysc-get (cons (- cur-level (car level-list)) 0))
                                                              result))
                                             (list ysc-lambda 1
                                                   (cons head-symbol
                                                         (cons (car result)
                                                               (cons (cont cur-level)
                                                                     (cdr result)))))))
                                       ))
                             (current (reverse exp)))
              
              (if (eq? (cdr current) '())
                  ;; tail expression
                  (cps-eval log context
                            result
                            level (car current))
                  (scan-recur
                   (let ((cont result)
                         (exp (car current)))
                     (lambda (cur-level)
                       (set! cur-level (+ 1 cur-level))
                       (set! args-level-list (cons cur-level args-level-list))
                       (list ysc-lambda 1
                             (cps-eval log context
                                       cont
                                       cur-level
                                       exp))))
                     (cdr current)
                     ))
                  )
              ))
            )

     ((eq? head-symbol ysc-apply-cc)
      (if (and (pair? exp)
               (eq? (cdr exp) '())
               )

            (cps-eval log context
                      (lambda (cur-level)
                        (set! cur-level (+ 1 cur-level))
                        (list ysc-lambda 1
                              (list ysc-apply
                                    (list ysc-lambda 1
                                          (list ysc-apply
                                                (list ysc-get 1 0) (list ysc-get 0 0) (list ysc-get 0 0)))
                                    (cont cur-level)
                                    ))
                        )
                      level (car exp))

          (log 'complain-error
               "Syntax error with @apply-cc")
          )
      )

     ((eq? head-symbol ysc-with-macro)

      (context
       (lambda (mx-mode? envir)
         ;; set a new macro binding level
         (set! envir (envir 'push-macro-level '()))
         
         (let recur ((current exp))
           (if (pair? (cdr current))
               ;; compile the expression converted
               (cps-eval
                log
                (make-context mx-mode? envir)
                cont
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

(define cps-eval
  (lambda (log context cont level-size oexp)

    (call-with-context
     (lambda (context exp)

       (cond

        ((symbol? exp)
         (cps-system-macro log context cont level-size ysc-get exp))
        
        ((not (pair? exp))
         (cps-system-macro log context cont level-size ysc-quote exp))

        (else

         (call-with-context
          (lambda (head-context
                   head-exp)

            (let ((head (if (system-macro-head? head-exp)
                            head-exp
                            (context-lookup head-context head-exp))))

              (cond
               
               ((symbol? head)            ; system macro
                (cps-system-macro log context cont
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
                            (cps-eval log (context
                                           (lambda (mx-mode? envir)
                                             (make-context #t envir)))
                                      cont
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
                        (cps-eval log (context
                                       (lambda (mx-mode? envir)
                                         (make-context #t envir)))
                                  cont
                                  level-size result)
                        )
                      (log 'complain-error "Cannot apply the macro"))))
               
               ))
            ) context (car exp))
         
         ))
       
       )

     context oexp)))

(define cps-with-system-envir
  (lambda (exp)

    (call-with-current-continuation
     (lambda (return)
       
       (cps-eval (lambda (op . args)
                       
                       (cond
                        ((eq? op 'complain-error)
                         (display "Error while compiling : ")
                         (display (car args))
                         (newline)
                         
                         (return '())))
                       )
                     (make-context #f system-envir)
                     (lambda (new)
                       (list 'exit))
                     0
                     exp)
       ))

    ))

;; cps.scm ends here
