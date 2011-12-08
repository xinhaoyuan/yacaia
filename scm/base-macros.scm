
;; base-macros.scm
;;
;; define several basic macros

(add-macro! system-envir

            'apply

            '(etc)
            
            '(etc ...)
            '(@apply-tl (@expand etc))
            )

(add-macro! system-envir

            'define

            '(name value args cmds)
            
            ;; '((name . args) . cmds)
            ;; '(@set! name (@native (@list lambda (@list (@expand args)) (@expand cmds))))

            '((name . args) . cmds)
            '(@set! name
                    (@list letrec
                           (@native (@list (@list name
                                                  (@list lambda
                                                         (@list (@expand args))
                                                         (@expand cmds)))))
                           name))


            '(name value)
            '(@set! name value)
            )

(add-macro! system-envir

            'letrec

            '(name value cmd)
            
            '(((name value) ...) cmd ...)
            '((@native
               (@list @lambda (@list (@expand name))
                      (@expand (@list @set! name value))
                      (@expand cmd)
                      ))
              (@expand (@hide name) #f)
              )
            )

(add-macro! system-envir

            'let

            '(proc-name name value cmd)

            '(((name value) ...) cmd ...)
            '((@native
               (@list @lambda (@list (@expand name))
                      (@expand cmd)
                      ))
              (@expand value))

            '(proc-name ((name value) ...) cmd ...)
            '(letrec (@list
                      (@list proc-name
                             (@list @lambda (@list (@expand name))
                                    (@native (@expand cmd)))))
               (@list (@native proc-name) (@expand value)))
            )

(add-macro! system-envir

            'and

            '(first etc)

            '(first)
            '(@begin first)
            
            '(first etc ...)
            '(@if first (@list and (@expand etc)) #f)
            )

(add-macro! system-envir

            'or

            '(first etc)

            '(first)
            '(@begin first)

            '(first etc ...)
            '(let (@list (@list result first))
               (@list
                @if (@native result) (@native result)
                (@list or (@expand etc))))
            )

(add-macro! system-envir

            'begin

            '(cmd)
            
            '(cmd ...)
            '(@begin (@expand cmd))
            )

(add-macro! system-envir

            'lambda

            '(arg-list cmd)
            
            '(arg-list . cmd)
            '(@lambda arg-list (@native (@expand cmd)))
            
            )

(add-macro! system-envir

            'if

            '(condition if-true if-false)

            '(condition if-true)
            '(@if condition if-true #f)

            '(condition if-true if-false)
            '(@if condition if-true if-false)
            )

(add-macro! system-envir

            'cond

            '(condition cmd etc)

            '((else
               cmd ...))
            '(@begin (@expand cmd))

            '((condition
               cmd ...))
            '(@if condition (@list @begin (@expand cmd)) #f)
            
            '((condition
               cmd ...)
              etc ...)
            '(@if condition (@list @begin (@expand cmd)) (@list cond (@expand etc)))
            )

(add-macro! system-envir

            'set!

            '(name value)
            
            '(name value)
            '(@set! name value)
            )

(add-macro! system-envir

            'call-with-current-continuation

            '(exp)

            '(exp)
            '(@apply-cc exp)
            )

;; base-macros.scm ends here.
