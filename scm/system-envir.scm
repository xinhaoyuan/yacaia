;; system-envir.scm

(add-ruledef! system-envir
              
              ysc-apply-proc
              
              '(proc args)

              '(proc args ...)
              (list ysc-apply 'proc '(@expand args))

              )

(add-ruledef! system-envir

              'apply

              '(etc)
              
              '(etc ...)
              '(@apply-tl (@expand etc))
              )

(add-ruledef! system-envir

              'define

              '(name value args cmds)
              
              ;; '((name . args) . cmds)
              ;; '(@set! name (@native (@list lambda (@list (@expand args)) (@expand cmds))))

              '((name . args) . cmds)
              '($set! name
                      (@list letrec
                             (@native (@list (@list name
                                                    (@list lambda
                                                           (@list (@expand args))
                                                           (@expand cmds)))))
                             name))


              '(name value)
              '($set! name value)
              )

(add-ruledef! system-envir

              'letrec

              '(name value cmd)
              
              '(((name value) ...) cmd ...)
              '((@native
                 (@list $lambda (@list (@expand name))
                        (@expand (@list $set! name value))
                        (@expand cmd)
                        ))
                (@expand (@hide name) #f)
                )
              )

(add-ruledef! system-envir

              'let

              '(proc-name name value cmd)

              '(((name value) ...) cmd ...)
              '((@native
                 (@list $lambda (@list (@expand name))
                        (@expand cmd)
                        ))
                (@expand value))

              '(proc-name ((name value) ...) cmd ...)
              '(letrec (@list
                        (@list proc-name
                               (@list $lambda (@list (@expand name))
                                      (@native (@expand cmd)))))
                 (@list (@native proc-name) (@expand value)))
              )

(add-ruledef! system-envir

              'and

              '(first)

              '(first ...)
              '($and (@expand first))
              )

(add-ruledef! system-envir

              'or

              '(first)

              '(first ...)
              '($or (@expand first))
              )

(add-ruledef! system-envir

              'begin

              '(cmd)
              
              '(cmd ...)
              '($begin (@expand cmd))
              )

(add-ruledef! system-envir

              'lambda

              '(arg-list cmd)
              
              '(arg-list . cmd)
              '($lambda arg-list (@native (@expand cmd)))
              
              )

(add-ruledef! system-envir

              'if

              '(condition if-true if-false)

              '(condition if-true)
              '($if condition if-true #f)

              '(condition if-true if-false)
              '($if condition if-true if-false)
              )

(add-ruledef! system-envir

              'cond

              '(condition cmd etc)

              '((else
                 cmd ...))
              '($begin (@expand cmd))

              '((condition
                 cmd ...))
              '($if condition (@list $begin (@expand cmd)) #f)
              
              '((condition
                 cmd ...)
                etc ...)
              '($if condition (@list $begin (@expand cmd)) (@list cond (@expand etc)))
              )

(add-ruledef! system-envir

              'set!

              '(name value)
              
              '(name value)
              '($set! name value)
              )

(add-ruledef! system-envir

              'call-with-current-continuation

              '(exp)

              '(exp)
              '($apply-cc exp)
              )

(set! system-envir (system-envir 'push-binding-level '()))

(system-envir 'add-binding! 'foo (make-binding-meta-data 0 0 type-atomic))
(system-envir 'add-binding! 'bar (make-binding-meta-data 0 1 type-object))

;; system-envir.scm ends here.
