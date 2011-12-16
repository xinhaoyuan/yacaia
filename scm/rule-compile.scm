;; rule-compile.scm
;; 
;; convert rules into internal representation

;; Internal symbols used
(define tag-etc      '...)

(define tag-list     'l)
(define tag-expand   'e)
(define tag-constant 'c)
(define tag-label    'v)
(define tag-hide     'h)
(define tag-native   'n)
(define tag-protect  'p)

(define tag-list?
  (lambda (tag) (or (eq? tag '@list) (eq? tag '@l))))
(define tag-expand?
  (lambda (tag) (or (eq? tag '@expand) (eq? tag '@e))))
(define tag-hide?
  (lambda (tag) (or (eq? tag '@hide) (eq? tag '@h))))
(define tag-native?
  (lambda (tag) (or (eq? tag '@native) (eq? tag '@n))))
(define tag-protect?
  (lambda (tag) (or (eq? tag '@protect) (eq? tag '@p))))

(define (compile-hygienic-rule

         context

         return-error
         is-label?

         input-rule                     ; with context
         output-rule                    ; with context
         )

  (letrec ((current-label-level 0)
           
           (result (make-plist-writer))
           (write-result (lambda (exp) (result 'write! exp)))
           
           (recur-enter (lambda () (result 'push-level!)))
           (recur-exit  (lambda () (result 'pop-level!)))

           (inc-label-level
            (lambda ()
              (set! current-label-level
                    (+ current-label-level 1))))

           (dec-label-level
            (lambda ()
              (set! current-label-level
                    (- current-label-level 1))))
           
           (get-result-clear
            (lambda ()
              (let ((ret (result 'finish!)))
                
                (set! result (make-plist-writer))
                ret)
              ))

           ;; processing of labels
           (label-size 0)
           (label-table '())
           (find-label (lambda (name) (assq name label-table)))
           (add-label  (lambda (name)
                         (let ((result (cons name
                                             (cons current-label-level label-size))))
                           (set! label-size (+ label-size 1))
                           (set! label-table (cons result
                                                   label-table))
                           result)))

           (mark-label  (lambda (label) (set-cdr! (cdr label) #f)))
           (label-id    (lambda (label) (cdr (cdr label))))
           (label-level (lambda (label) (car (cdr label))))
           
           ;; temporary place for result
           (compiled-input-rule #f) (compiled-output-rule #f)

           ;; error reporter
           (log (compiling-log))
           (error (deque))
           (complain-error
            (lambda (msg)

              ;; (display msg) (newline)
              (error 'push-tail!
                     (cons msg log))
              #f
              ))
           ;; warning reporter
           (warning (deque))		
           (complain-warning
            (lambda (msg)
              (warning 'push-tail!
                       (cons msg log))
              ))
           )

    ;; ----------------------------------------
    
    (letrec

        ;; compile the input rule here
        ;; the vaule #t indicate the success, #f means failed
        ((compile-input-rule
          (lambda (context exp)

            (cond
             
             ((symbol? exp)
              (if (is-label? exp)
                  (if (find-label exp)	; ensure the label did not exist before
                      (complain-error "Syntax label duplicated")
                      (begin
                        (write-result
                         (list tag-label
                               (label-id (add-label exp))))
                        #t))
                  (begin
                    (write-result	    ; set as a constant matching
                     ;; purge the context for matching
                     (list tag-constant (purge-exp context exp)))
                    #f)))
             
             ((pair? exp)
              (let ((result #f))
                
                (recur-enter)
                (write-result tag-list)

                (let recur ((current (plist->dplist exp)))
                  
                  (cond
                   ;; check for tail expression
                   ((or (dpl-tail? current)
                        ;; consider the ETC symbol as tail
                        (let ((next (dpl-next current)))
                          (and (not (dpl-tail? next))
                               (dpl-tail? (dpl-next next))
                               (eq? (purge context (dpl-read next)) tag-etc))))
                    
                    (if (eq? (dpl-read current) '())
                        ;; no tail expression
                        (write-result '())
                        ;; process the tail expression
                        (begin
                          (push-comp-log! log "Compile tail-pattern" current)
                          (inc-label-level)
                          
                          (if (not (call-with-context compile-input-rule
                                                      context
                                                      (dpl-read current)))
                              (complain-warning
                               "There isn't any syntax label in a tail-pattern")
                              (set! result #t))
                          
                          (dec-label-level)
                          
                          (pop-comp-log! log)
                          )))
                   
                   (else
                    (push-comp-log! log "Compile sub rule" current)

                    (and (call-with-context compile-input-rule
                                            context
                                            (dpl-read current))
                         (set! result #t))
                    
                    (pop-comp-log! log)
                    
                    (recur (dpl-next current))
                    ))
                  )
                
                (recur-exit)
                result))
             
             (else (complain-error "Unexpected type of element"))

             )
            
            )))

      (push-comp-log! log "Compile input rule"
                      (cons
                       input-rule
                       (cons '() '())))

      (call-with-context compile-input-rule
                         context
                         input-rule)
      
      (pop-comp-log! log)
      (set! compiled-input-rule (get-result-clear))
      
      (and (not (eq? compiled-input-rule '()))
           (set! compiled-input-rule (car compiled-input-rule)))
      )

    (letrec

        ;; compile output rule
        ;; ??? chinese comment
        ;; 返回结果同样是编译的表达式中有没有出现标签匹配
        ((compile-output-rule
          
          (lambda (free-context? context native? exp)
            
            (cond
             
             ((and (list? exp) (pair? exp))
              (let ((head (purge context (car exp))))
                (cond
                 ((tag-list? head)
                  (if (pair? (cdr exp))
                      (let ((result #f))
                        
                        (recur-enter)
                        (write-result tag-list)
                        
                        (let recur ((current (plist->dplist (cdr exp))))

                          (if (dpl-tail? current)
                              (begin
                                (recur-exit)
                                result)
                              (begin
                                (push-comp-log! log "Compile sub rule in list pattern" current)
                                
                                (and (call-with-free-context compile-output-rule
                                                             context
                                                             native?
                                                             (dpl-read current)
                                                             )
                                     (set! result #t))

                                (pop-comp-log! log)
                                (recur (dpl-next current)))
                              )))

                      (complain-error "Unexpected end of list pattern")))

                 ((tag-expand? head)
                  (if (pair? (cdr exp))
                      (let ((result #f))

                        (recur-enter)
                        (inc-label-level)
                        (write-result tag-expand)

                        (let recur ((current (plist->dplist (cdr exp))))

                          (if (dpl-tail? current)

                              (begin
                                
                                (and (not result)
                                     (complain-error
                                      "There must be at least one syntax in expand pattern"))

                                (recur-exit)
                                (dec-label-level)
                                result)
                              
                              (begin
                                (push-comp-log! log "Compile sub rule in expand pattern" current)
                                
                                (and (call-with-free-context compile-output-rule
                                                             context
                                                             native?
                                                             (dpl-read current))
                                     (set! result #t))
                                
                                (pop-comp-log! log)

                                (recur (dpl-next current)))
                              )))
                      (complain-error "Unexpected end of expand pattern"))
                  )

                 ((tag-native? head)
                  (if (and (pair? (cdr exp)) (eq? (cdr (cdr exp)) '()))
                      (call-with-free-context compile-output-rule
                                              context
                                              #t
                                              (car (cdr exp)))
                      (complain-error
                       "Format error with native pattern"))
                  )

                 ((tag-protect? head)
                  (if (and (pair? (cdr exp)) (eq? (cdr (cdr exp)) '()))
                      (call-with-free-context compile-output-rule
                                              context
                                              #f
                                              (car (cdr exp)))
                      (complain-error
                       "Format error with protect pattern"))
                  )
                 
                 
                 ((tag-hide? head)
                  (if (and (pair? (cdr exp)) (eq? (cdr (cdr exp)) '()))
                      (begin
                        (recur-enter)
                        (write-result tag-hide)
                        
                        (call-with-free-context compile-output-rule
                                                context
                                                native?
                                                (car (cdr exp)))
                        
                        (recur-exit))
                      (complain-error
                       "Format error with hide pattern"))
                  )
                 
                 (else
                  (complain-error "Unknown head of sub pattern"))
                 )))

             (else                      ; for symbol and constant
              (if (and (symbol? exp) (is-label? exp))
                  (let ((find-result (find-label exp)))
                    (if find-result     ; 是否只存在
                        ;;(if (label-id find-result) ; 是否只出现过一次
                        (if (eq? (label-level find-result) current-label-level)
                            (begin
                              (write-result
                               (list tag-label
                                     native?
                                     (label-id find-result)))
                              ;; OUTMAC fix -- (mark-label find-result) ; 标记为已经出现过
                              #t)
                            
                            (complain-error
                             "Level of label does not match")
                            )
                        
                        ;;(complain-error
                        ;;"Syntax label duplicated in output rule"))

                        (complain-error "Syntax label does not occur in the output rule")
                        ))
                  (begin
                    (write-result
                     ;; write constant, consider the context
                     (list tag-constant
                           ;; (if free-context?
                           ;;     (cons '() exp)
                           (if native?
                               (make-exp-with-context #f (purge-exp context exp))
                               (make-exp-with-context context exp))))
                    ;; ))
                    #f)))
             )
            
            )))
      
      (push-comp-log! log "Compile output rule"
                      (cons output-rule
                            (cons '() '())))

      (call-with-free-context
       ;;        compile-output-rule
       (lambda (free-context? context native? exp)
         (compile-output-rule free-context?
                              context
                              native?
                              (cons
                               (context
                                (lambda (mx-mode? envir)
                                  (if mx-mode?
                                      (cons '()
                                            '@list)
                                      '@list)))
                               exp)
                              )
         )
       context
       #f	; protect the environment by default
       output-rule)
      
      (pop-comp-log! log)
      (set! compiled-output-rule (get-result-clear))
      (and (not (eq? compiled-output-rule '()))
           (set! compiled-output-rule (car compiled-output-rule)))
      )

    (if (error 'empty?)
        (cons label-size
              (cons compiled-input-rule
                    compiled-output-rule))

        (for-each return-error
                  (error 'plist))
        
        )

    ))

;; rule-compile.scm ends here.
