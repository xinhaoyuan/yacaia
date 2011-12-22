;; cps.scm

(define system-rule-head?
  (make-fixed-set
   (list ysc-begin
         ysc-if
         ysc-lambda
         ysc-set!
         ysc-apply
         ysc-inline-apply
         ysc-apply-tl
         ysc-apply-cc
         ysc-with-rule
         ysc-quote)))

(define cps-system-rule
  (lambda (log
           context
           cont                         ; (cont level type) return the
                                        ; continuation exp under
                                        ; level, add accept the value
                                        ; of type
           
           level                        ; level of binding if use cont
           ret-put                      ; (ret-put put), put: (lambda (level type))
           head-symbol
           exp                          ; without context
           )

    (cond

     ((eq? head-symbol ysc-quote)
      ((ret-put (lambda (level type)
                  (list type ysc-quote (purge-exp context (car exp)))))
       level
       ;; XXX Get the type of the constance
       type-void)
      )

     ((eq? head-symbol ysc-get)
      (let ((result
             (call-with-context
              (lambda (context name)
                (context-lookup-binding context name))
              context exp)
             ))

        (if (eq? result '())
            (log 'complain-error "Cannot find the binding!")
            ((ret-put (lambda (level type)
                        (list type
                              ysc-get (if result
                                          ;; get the meta-data of the binding
                                          (cdr (cdr result))
                                          exp))))
             level (binding-typedef (cdr (cdr result)))))
        ))

     ((eq? head-symbol ysc-set!)
      (apply
       (lambda (name value)
         
         (let ((result
                (call-with-context
                 (lambda (context name)
                   (context-lookup-binding context name))
                 context name)))

           (if (eq? result '())
               (log 'complain-error "Cannot find the binding")

               (cps-eval log context
                         
                         (lambda (dlevel dtype)
                           (set! dlevel (+ 1 dlevel))
                           (list (type-typed-lambda-cps (list dtype))
                                 ysc-lambda
                                 ((ret-put (lambda (level type)
                                             ;; set always return the data in binding
                                             (list (binding-typedef (cdr (cdr result)))
                                                   ysc-set!
                                                   (cdr (cdr result))
                                                   (list ysc-get (make-binding-meta-data
                                                                  dlevel 0
                                                                  dtype)))))
                                  dlevel dtype)
                                 ))
                         level
                         
                         (lambda (put)
                           (ret-put (lambda (level type)
                                      ;; set always return the data in binding
                                      (list (binding-typedef (cdr (cdr result)))
                                            ysc-set!
                                            (cdr (cdr result))
                                            (put level type)
                                            ))))
                         value
                         ))))
       exp))

     ((eq? head-symbol ysc-begin)
      (if (eq? exp '())
          (log 'complain-error "the Begin expression cannot be empty")
          ;; pass the check
      	  (let eval-recur ((current-cont cont)
                           (current-ret-put ret-put)
                           (current-exp (reverse exp)))
            
            (if (eq? (cdr current-exp) '())
                ;; tail expression
                (cps-eval log context
                          current-cont
                          level
                          current-ret-put
                          (car current-exp))
                
                (eval-recur
                 (lambda (level type)
                   (list (type-typed-lambda-cps (list type-atomic))
                         ysc-lambda
                         (cps-eval log context
                                   current-cont
                                   (+ 1 level)
                                   current-ret-put
                                   (car current-exp))))
                 (lambda (put)
                   (lambda (level type)
                     (let ((result
                            (cps-eval log context
                                      current-cont
                                      level
                                      current-ret-put
                                      (car current-exp))))
                       (if (eq? (car (cdr result)) ysc-begin)
                           (cons type (cons ysc-begin (cons (put level type) (cdr (cdr result)))))
                           (list type ysc-begin (put level type) result))
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
           ((ret-put
             (lambda (level type)
               (set! level (+ 1 level))
               (set! envir (envir 'push-binding-level '()))
               ;; first variable is the cont
               (envir 'add-binding! '() (make-binding-meta-data level 0 type-object))
               (let recur ((offset 1)
                           (cur args-list))
                 (if (pair? cur)
                     (begin
                       (envir 'add-binding! (car cur) (make-binding-meta-data level offset type-object))
                       (recur (+ 1 offset) (cdr cur)))))
               
               (list type
                     ysc-lambda

                     (cps-system-rule
                      log
                      (make-context mx-mode? envir)
                      
                      ;; get the continuation from arg 0
                      (lambda (new-level type)
                        (list type-object ysc-get (make-binding-meta-data level 0 type-object)))
                      
                      (+ 1 level)

                      (lambda (nput)
                        (lambda (new-level type)
                          (list type-void
                                ysc-apply
                                (list type-object ysc-get (make-binding-meta-data level 0 type-object))
                                (nput level type))))
                      
                      ysc-begin
                      codes-list
                      ))
               ;; one more for continuation
               )) level (type-lambda-cps (+ 1 args-count)))
           ))))

     ((eq? head-symbol ysc-if)
      (apply
       (lambda (condition if-true if-false)
         (let* ((cont-saved (cont level type-object))
                ;; get the continuation type
                (cont-type (car cont-saved))
                (accept-type (type-lambda-arg cont-type 1)
                             type-object
                             ))
           
           (set! level (+ 1 level))
           (letrec ((inner-cont
                     (lambda (inner-level inner-type)
                       (list cont-type
                             ysc-get (make-binding-meta-data level 0 cont-type))))
                    
                    (inner-ret-put
                     (lambda (put)
                       (lambda (inner-level inner-type)
                         (list type-void
                               ysc-apply
                               (list cont-type ysc-get (make-binding-meta-data level 0 cont-type))
                               (put inner-level accept-type)))))
                    
                    (cond-cont
                     (lambda (cond-level cond-type)
                       (set! cond-level (+ 1 cond-level))
                       ;; if always accept cond as atomic
                       (list (type-typed-lambda-cps (list type-atomic))
                             ysc-lambda
                             (list ysc-if (list type-atomic ysc-get (make-binding-meta-data cond-level 0 type-atomic))
                                   (cps-eval log context inner-cont cond-level inner-ret-put if-true)
                                   (cps-eval log context inner-cont cond-level inner-ret-put if-false)))))
                    
                    (cond-ret-put
                     (lambda (put)
                       (lambda (cond-level cond-type)
                         (list type-void
                               ysc-if (put cond-level type-atomic)
                               (cps-eval log context inner-cont cond-level inner-ret-put if-true)
                               (cps-eval log context inner-cont cond-level inner-ret-put if-false)))))
                    )
             (list type-void
                   ysc-apply
                   (list (type-typed-lambda-cps (list cont-type))
                         ysc-lambda
                         (cps-eval log context cond-cont level cond-ret-put condition))
                   cont)))
         ) exp))

     ((or (eq? head-symbol ysc-apply)
          (eq? head-symbol ysc-apply-tl)
          (eq? head-symbol ysc-inline-apply)
          )
      (if (not (pair? exp))
          (log 'complain-error "Cannot apply since incorrect format")

          ;; pass the check
          (letrec ((args-list '())
                   (apply-line (list head-symbol))
                   (gen-apply-line
                    (lambda (level)
                      (let gen-recur
                          ((args args-list)
                           (result '()))
                        (if (pair? args)
                            (gen-recur (cdr args)
                                       (cons (car args) result))
                            (cons type-void
                                  (cons head-symbol
                                        (cons (car result)
                                              (cons (cont level (type-lambda-arg (car result) 1))
                                                    (cdr result)))))))))
                   (gen-inline-apply-line
                    (lambda (level)
                      (let gen-recur
                          ((args args-list)
                           (result '()))
                        (if (pair? args)
                            (gen-recur (cdr args)
                                       (cons (car args) result))
                            (cons (type-lambda-ret (car (car result)))
                                  (cons ysc-inline-apply
                                        (cons (car result)
                                              (cdr result))))))))
                   )
            
            (let scan-recur ((current-cont
                              (lambda (level type)
                                (set! level (+ 1 level))
                                (set! args-list (cons (list type
                                                            ysc-get (make-binding-meta level 0 type)) args-list))
                                (list (type-typed-lambda-cps (list type))
                                      ysc-lambda
                                      (if (eq? head-symbol ysc-inline-apply)
                                          ((ret-put gen-inline-apply-line) level)
                                          (gen-apply-line (+ 1 level))))
                                ))
                             (current-ret-put
                              (lambda (put)
                                (lambda (level type)
                                  (set! args-list (cons (put level type) args-list))
                                  (if (eq? head-symbol ysc-inline-apply)
                                      ((ret-put gen-inline-apply-line) level)
                                      (gen-apply-line level))
                                  )
                                ))
                             (current (reverse (if (eq? head-symbol ysc-inline-apply)
                                                   ;; the head of inline apply is the apply-type
                                                   (cdr exp)
                                                   exp))))
              
              (if (eq? (cdr current) '())
                  ;; tail expression
                  (cps-eval log context
                            current-cont
                            level
                            current-ret-put
                            (car current))
                  (scan-recur

                   (lambda (level type)
                     (set! level (+ 1 level))
                     (set! args-list (cons (list type
                                                 ysc-get (make-binding-meta level 0 type)) args-list))
                     (list (type-typed-lambda-cps (list type))
                           ysc-lambda
                           (cps-eval log context
                                     current-cont
                                     level
                                     current-ret-put
                                     (car current)
                                     )))

                   (lambda (put)
                     (lambda (level type)
                       (set! args-list (cons (put level type) args-list))
                       (cps-eval log context
                                 current-cont
                                 level
                                 current-ret-put
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
                    (lambda (level type)
                      (set! level (+ 1 level))
                      (let* ((cont-saved (cont level type-object))
                             (cont-type (car cont-saved)))
                        
                        (list (type-typed-lambda-cps (list type))
                              ysc-lambda
                              (list type-void
                                    ysc-apply
                                    (list (type-type-lambda-cps (list cont-type))
                                          ysc-lambda
                                          (list ysc-apply
                                                (list type ysc-get (make-binding-meta level 0 type))
                                                (list cont-type ysc-get (make-binding-meta (+ 1 level) 0 cont-type))
                                                (list cont-type ysc-get (make-binding-meta (+ 1 level) 0 cont-type))))
                                    cont-saved
                                  ))
                        ))
                    cont-level
                    (lambda (put)
                      (lambda (level type)
                        (let* ((cont-saved (cont level type-object))
                               (cont-type (car cont-saved)))
                          (set! level (+ 1 level))
                          (list type-void
                                ysc-apply
                                (list (type-type-lambda-cps (list cont-type))
                                      ysc-lambda
                                      (list ysc-apply
                                            (put level type)
                                            (list ysc-get (make-binding-meta level 0 cont-type))
                                            (list ysc-get (make-binding-meta level 0 cont-type))))
                                cont-saved
                                ))))
                    (car exp))

          (log 'complain-error
               "Syntax error with @apply-cc")
          )
      )

     ((eq? head-symbol ysc-with-rule)

      (context
       (lambda (mx-mode? envir)
         ;; set a new rule binding level
         (set! envir (envir 'push-ruledef-level '()))
         
         (let recur ((current exp))
           (if (pair? (cdr current))
               ;; compile the expression converted
               (cps-eval
                log
                (make-context mx-mode? envir)
                cont
                cont-level
                ret-put
                (car current))

               (begin
                 (apply
                  call-with-context
                  (lambda (context rule-name rule-labels . rules-list)
                    (set! rule-name (purge context rule-name))
                    (if (not (symbol? rule-name))
                        (log 'complain-error "Syntax error with rule name"))
                    
                    (set! rule-labels
                          (make-fixed-set
                           (call-with-context
                            (lambda (context list)
                              (purge-list context list))
                            context rule-labels)))
                    
                    (let ((result (deque)))
                      
                      (let recur ((current rules-list))
                        
                        (if (eq? current '())
                            (envir 'add-ruledef!
                                   (cons rule-name
                                         (result 'plist)))
                            (if (pair? (cdr current))
                                (log 'complain-error "Syntax error with rules list")
                                
                                (begin
                                  (result 'push-tail!
                                          (compile-hygienic-rule
                                           context
                                           (lambda (error) (log 'complain-error error))
                                           rule-labels
                                           (car current)
                                           (car (cdr current)))
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

;; find the first matchable rule and apply it
(define apply-rule
  (lambda (rule context exp)
    
    ;; (display "apply-rule ") (display rule) (newline)
    
    (let apply-recur ((current rule))

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
  (lambda (log context cont cont-level ret-put oexp)

    (call-with-context
     (lambda (context exp)

       ;; (display "CPS-EVAL: ") (display (purge-exp context exp)) (newline)
       
       (cond

        ((symbol? exp)
         (cps-system-rule log context cont cont-level ret-put ysc-get exp))
        
        ((not (pair? exp))
         (cps-system-rule log context cont cont-level ret-put ysc-quote (list exp)))

        (else

         (call-with-context
          (lambda (head-context
                   head-exp)

            (let ((head (if (system-rule-head? head-exp)
                            head-exp
                            (context-lookup head-context head-exp))))

              (cond
               
               ((symbol? head)            ; system rule
                (cps-system-rule log context cont cont-level ret-put
                                 head (cdr exp)
                                 ))
               
               ((or (not (pair? head))
                    (eq? (car head) meta-type-binding))
                (let ((result
                       (context-lookup-ruledef context ysc-apply-proc)))
                  (if result
                      (begin
                        (set! result (apply-rule (cdr (cdr result))
                                                 context
                                                 exp))
                        (if result
                            (cps-eval log (context
                                           (lambda (mx-mode? envir)
                                             (make-context #t envir)))
                                      cont cont-level ret-put
                                      result)
                            (log 'complain-error
                                 "Cannot apply apply-proc rule!")
                            ))
                      (log 'complain-error
                           "The ``apply-proc'' rule not exist in current context"))
                  ))
               
               ((eq? (car head) meta-type-ruledef)
                
                (let ((result (apply-rule (cdr (cdr head))
                                          context (cdr exp))))
                  (if result
                      (begin
                        (cps-eval log (context
                                       (lambda (mx-mode? envir)
                                         (make-context #t envir)))
                                  cont cont-level ret-put
                                  result)
                        )
                      (log 'complain-error "Cannot apply the rule"))))
               
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
                 (lambda (level type)
                   (set! level (+ 1 level))
                   (list (type-typed-lambda-cps (list type))
                         ysc-lambda
                         (list type-void ysc-exit (list type ysc-get (make-binding-meta-data level 0 type)))))
                 0
                 (lambda (cont)
                   (lambda (level type)
                     (list type-void ysc-exit (cont level type))))
                 exp)
       ))

    ))

;; cps.scm ends here
