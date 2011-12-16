;; system-rules.scm

(define add-ruledef!
  (lambda (envir name labels-list . rules)

    (let ((result (deque))
          (context
           (make-context #f
                         envir))
          )

      (let recur ((current rules))

        (if (eq? current '())
            (begin (envir 'add-ruledef! name (result 'plist))
                   (result 'plist))
            
            (begin
              (result 'push-tail!
                      (compile-hygienic-rule
                       
                       context
                       
                       (lambda (error)
                         (display (car error)) (newline)
                         (display ((cdr error) 'plist)) (newline)
                         )
                       
                       (make-fixed-set labels-list)
                       
                       (car current)
                       (car (cdr current)))
                      )
              
              (recur (cdr (cdr current)))
              )))
      )))

;; system-rules.scm ends here.
