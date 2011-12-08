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
           cont                         ; (cont cur-level) return the continuation exp under cur-level
           cont-level                   ; level of binding if use cont
           ret-cont                     ; (ret-cont cont)
           head-symbol
           exp                          ; without context
           )

    (cond

     ((eq? head-symbol ysc-quote)
      ((ret-cont (lambda (level)
                   (list ysc-quote (purge-exp context (car exp))))) cont-level)
      )

     ((eq? head-symbol ysc-get)
      (let ((result
             (call-with-context
              (lambda (context name)
                (context-lookup-variable context name))
              context exp)
             ))

        ((ret-cont (lambda (level)
                     (list ysc-get (if result
                                       (make-variable-access level (cdr result))
                                       exp)))) cont-level)
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

                     (lambda (dlevel)
                       (set! dlevel (+ 1 dlevel))
                       (list ysc-lambda 1
                             ((ret-cont (lambda (level)
                                          (list ysc-set!
                                                (if (symbol? variable)
                                                    variable
                                                    (make-variable-access level (cdr variable)))
                                                (list ysc-get (cons (- level dlevel) 0)))))
                              dlevel)
                             ))
                     cont-level

                     (lambda (cont)
                       (ret-cont (lambda (level)
                                   (list ysc-set!
                                         (if (symbol? variable)
                                             variable
                                             (make-variable-access level (cdr variable)))
                                         (cont level)
                                     ))))
                     
                     value
                     )))
       exp))

     ((eq? head-symbol ysc-begin)
      (if (eq? exp '())
          (log 'complain-error "the Begin expression cannot be empty")
          ;; pass the check
      	  (let eval-recur ((current-cont cont)
                           (current-ret-cont ret-cont)
                           (current-exp (reverse exp)))
            
            (if (eq? (cdr current-exp) '())
                ;; tail expression
                (cps-eval log context
                          current-cont
                          cont-level
                          current-ret-cont
                          (car current-exp))
                
                (eval-recur
                 (lambda (level)
                   (list ysc-lambda 1
                         (cps-eval log context
                                   current-cont
                                   (+ 1 level)
                                   current-ret-cont
                                   (car current-exp))))
                 (lambda (cont)
                   (lambda (level)
                     (let ((result
                            (cps-eval log context
                                     current-cont
                                     level
                                     current-ret-cont
                                     (car current-exp))))
                       (if (eq? (car result) ysc-begin)
                           (cons ysc-begin (cons (cont level) (cdr result)))
                           (list ysc-begin (cont level) result))
                           
                       )))
                 (cdr current-exp)
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

           ;; pass the check
           ((ret-cont
             (lambda (level)
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
                        (lambda (level)
                          (list ysc-get (cons (- level old) 0))))
                  
                      (+ 1 level)

                      (let ((old (+ 1 level)))
                        (lambda (ncont)
                          (lambda (level)
                            (list ysc-apply
                                  (list ysc-get (cons (- level old) 0))
                                  (ncont level)))))
                      
                      ysc-begin
                      codes-list
                      ))
               )) cont-level)
        ))))

     ((eq? head-symbol ysc-if)
      (apply
       (lambda (condition if-true if-false)
         (set! cont-level (+ 1 cont-level))
         (letrec ((inner-cont (lambda (level)
                                (list ysc-get (cons (- level cont-level) 0))))
                  (inner-ret-cont
                   (lambda (cont)
                     (lambda (level)
                       (list ysc-apply
                             (list ysc-get (cons (- level cont-level) 0))
                             (cont level)))))
                                    
                  (cond-cont
                   (lambda (level)
                     (list ysc-lambda 1
                           (list ysc-if (list ysc-get (cons 0 0))
                                 (cps-eval log context inner-cont (+ 1 level) inner-ret-cont if-true)
                                 (cps-eval log context inner-cont (+ 1 level) inner-ret-cont if-false)))))
                  
                  (cond-ret-cont
                   (lambda (cont)
                     (lambda (level)
                       (list ysc-if (cont level)
                             (cps-eval log context inner-cont level inner-ret-cont if-true)
                             (cps-eval log context inner-cont level inner-ret-cont if-false)))))
                  )
           (list ysc-apply
                 (list ysc-lambda 1
                       (cps-eval log context cond-cont cont-level cond-ret-cont condition))
                 (cont (- cont-level 1))))
         ) exp))

     ((or (eq? head-symbol ysc-apply)
          (eq? head-symbol ysc-apply-tl)
          (eq? head-symbol ysc-inline-apply)
          )
      (if (not (pair? exp))
          (log 'complain-error "Cannot apply since incorrect format")

          ;; pass the check
          (letrec ((args-list '())
                   (args-size 0)
                   (apply-line (list head-symbol))
                   (gen-apply-line
                    (lambda (level)
                      (let gen-recur
                          ((args args-list)
                           (result '()))
                        (if (pair? args)
                            (gen-recur (cdr args)
                                       (if (number? (car args))
                                           (cons (list ysc-get (cons (- level (car args)) 0))
                                                 result)
                                           (cons (car args) result)))
                            (cons head-symbol
                                  (cons (car result)
                                        (cons (cont level)
                                              (cdr result))))))))
                   (gen-inline-apply-line
                    (lambda (level)
                      (let gen-recur
                          ((args args-list)
                           (result '()))
                        (if (pair? args)
                            (gen-recur (cdr args)
                                       (if (number? (car args))
                                           (cons (list ysc-get (cons (- level (car args)) 0))
                                                 result)
                                           (cons (car args) result)))
                            (cons ysc-inline-apply
                                  (cons (car result)
                                        (cdr result)))))))
                   )
            
            (let scan-recur ((current-cont
                              (lambda (level)
                                (set! level  (+ 1 level))
                                (set! args-size (+ 1 args-size))
                                (set! args-list (cons (+ 1 level) args-list))
                                (list ysc-lambda 1
                                      (if (eq? head-symbol ysc-inline-apply)
                                          ((ret-cont gen-inline-apply-line) level)
                                          (gen-apply-line (+ 1 level))))
                                ))
                             (current-ret-cont
                              (lambda (cont)
                                (lambda (level)
                                  (set! args-size (+ 1 args-size))
                                  (set! args-list (cons (cont level) args-list))
                                  (if (eq? head-symbol ysc-inline-apply)
                                      ((ret-cont gen-inline-apply-line) level)
                                      (gen-apply-line level))
                                  )
                                ))
                             (current (reverse exp)))
              
              (if (eq? (cdr current) '())
                  ;; tail expression
                  (cps-eval log context
                            current-cont
                            cont-level
                            current-ret-cont
                            (car current))
                  (scan-recur

                   (lambda (level)
                     (set! args-size (+ 1 args-size))
                     (set! args-list (cons (+ 1 level) args-list))
                     (list ysc-lambda 1
                           (cps-eval log context
                                     current-cont
                                     (+ 1 level)
                                     current-ret-cont
                                     (car current)
                                     )))

                   (lambda (cont)
                     (lambda (level)
                       (set! args-size (+ 1 args-size))
                       (set! args-list (cons (cont level) args-list))
                       (cps-eval log context
                                 current-cont
                                 level
                                 current-ret-cont
                                 (car current)
                                 )))
                   
                   (cdr current)
                   )
                  )
              )
              )))

     ((eq? head-symbol ysc-apply-cc)
      (if (and (pair? exp)
               (eq? (cdr exp) '())
               )

          (cps-eval log context
                    (lambda (level)
                      (set! level (+ 1 level))
                      (list ysc-lambda 1
                            (list ysc-apply
                                  (list ysc-lambda 1
                                        (list ysc-apply
                                              (list ysc-get (cons 1 0)) (list ysc-get (cons 0 0)) (list ysc-get (cons 0 0))))
                                  (cont level)
                                  ))
                      )
                    cont-level
                    (lambda (ncont)
                      (lambda (level)
                        (list ysc-apply
                              (list ysc-lambda 1
                                    (list ysc-apply
                                          (ncont (+ 1 level)) (list ysc-get (cons 0 0)) (list ysc-get (cons 0 0))))
                              (cont level))))
                    (car exp))

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
                cont-level
                ret-cont
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
         ))
      )
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
  (lambda (log context cont cont-level ret-cont oexp)

    (call-with-context
     (lambda (context exp)

       ;; (display "CPS-EVAL: ") (display (purge-exp context exp)) (newline)
       
       (cond

        ((symbol? exp)
         (cps-system-macro log context cont cont-level ret-cont ysc-get exp))
        
        ((not (pair? exp))
         (cps-system-macro log context cont cont-level ret-cont ysc-quote (list exp)))

        (else

         (call-with-context
          (lambda (head-context
                   head-exp)

            (let ((head (if (system-macro-head? head-exp)
                            head-exp
                            (context-lookup head-context head-exp))))

              (cond
               
               ((symbol? head)            ; system macro
                (cps-system-macro log context cont cont-level ret-cont
                                  head (cdr exp)
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
                                      cont cont-level ret-cont
                                      result)
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
                                  cont cont-level ret-cont
                                  result)
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
                     (lambda (level)
                       (list 'exit-proc))
                     0
                     (lambda (cont)
                       (lambda (level)
                         (list 'exit (cont level))))
                     exp)
       ))

    ))

;; cps.scm ends here
