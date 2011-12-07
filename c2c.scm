;; c2c.scm
;;
;; convert CPS into C snippets

(define c2c-head-string
  "\
#include \"headers.h\"
")
(define c2c-tail-string "")

(define (make-c2c-context)
  (cons 0 ""))

(define c2c-context-dump cdr)
(define c2c-context-dump-set! set-cdr!)

(define c2c-context-lambda-start car)
(define c2c-context-lambda-start-set! set-car!)

(define (c2c c2c-context main-name cps-exp)
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
           (lambda-constant-vec (make-vector (vector-length lambda-vec) '()))
           (lambda-start (c2c-context-lambda-start c2c-context))
           (next-offset 1)
           (offset-stack (list 0))

           (serialize-to-string
            (lambda (exp)
              (cond
               ((symbol? exp)
                (symbol->string exp))

               ((number? exp)
                (number->string exp))

               ((string? exp) exp)

               ((pair? exp)
                (let ((a (serialize-to-string (car exp)))
                      (b (serialize-to-string (cdr exp))))
                  
                  (string-append a b)
                  ))

               (else ""))))
           )
    
    (vector-set! lambda-vec 0 
                 (let recur ((exp cps-exp))
                   (cond
                    
                    ((eq? (car exp) ysc-lambda)
                     (apply (lambda (argc body)

                              (let ((id (+ next-offset lambda-start)))
                                (set! offset-stack (cons next-offset offset-stack))
                                (set! next-offset (+ 1 next-offset))
                                (vector-set! lambda-vec (- id lambda-start) (recur body))
                                (set! offset-stack (cdr offset-stack))
                                (list "create_lambda(context,lambda_" id "," argc ")")
                                ))
                            
                            (cdr exp)))

                    ((eq? (car exp) ysc-set!)
                     (apply (lambda (ref value)

                              (set! value (recur value))
                              
                              (if (pair? ref)
                                  (list "set_local(context," (car ref) "," (cdr ref) "," value ")")
                                  (list "set_global(context," (quote-string (symbol->string ref)) "," value ")")
                                  )

                              ) (cdr exp)))

                    ((or (eq? (car exp) ysc-apply)
                         (eq? (car exp) ysc-apply-tl))
                     (let ((writer (make-plist-writer)))
                       (writer 'push-level!)

                       (cond
                        ((eq? (car exp) ysc-apply)
                         (writer 'write! "apply(context"))
                        ((eq? (car exp) ysc-apply-tl)
                         (writer 'write! "apply_tl(context")))

                       (let inner-recur ((cur (cdr exp)))

                         (if (pair? cur)
                             (begin
                               (writer 'write! ",")
                               (writer 'write! (recur (car cur)))
                               (inner-recur (cdr cur)))))
                       (writer 'write! ")")
                       (car (writer 'finish!))
                       ))

                    ((eq? (car exp) ysc-inline-apply)
                     (apply (lambda (inline-no . args)
                              (let ((writer (make-plist-writer)))
                                (writer 'push-level!)
                                (writer 'write! "__INLINE_" (car (cdr inline-no)) "(context")

                                (let inner-recur ((cur args))

                                  (if (pair? cur)
                                      (begin
                                        (writer 'write! ",")
                                        (writer 'write! (recur (car cur)))
                                        (inner-recur (cdr cur)))))
                                (writer 'write! ")")
                                (car (writer 'finish!))
                                )) (cdr exp)))

                    ((eq? (car exp) ysc-if)
                     (apply (lambda (if-cond if-true if-false)

                              (list "if (is_ture(" (recur if-cond) ")) {"
                                    (recur if-true) ";} else {" (recur if-false) ";}")

                              ) (cdr exp)))

                    ((eq? (car exp) ysc-begin)
                     (let inner-recur ((cur (cdr exp))
                                       (result (list "({")))
                       (if (pair? cur)
                           (inner-recur (cdr cur)
                                        (cons (recur (car cur))
                                              (if (eq? (cdr result) '())
                                                  result
                                                  (cons ";" result))
                                              ))
                           (reverse (cons ";})" result))))
                     )


                    ((eq? (car exp) 'exit) (list "__EXIT(context," (recur (car (cdr exp))) ")"))
                    ((eq? (car exp) 'exit-proc) "__EXIT_PROC")

                    ((eq? (car exp) ysc-get)
                     (apply (lambda (ref)

                              (if (pair? ref)
                                  (list "get_local(context," (car ref) "," (cdr ref) ")")
                                  (list "get_global(context," (quote-string (symbol->string ref)) ")"))
                              
                              ) (cdr exp)))

                    ((eq? (car exp) ysc-quote)
                     (apply (lambda (value)

                              (letrec ((offset (car offset-stack))
                                       (cexp
                                        (cond
                                         ((number? value)
                                          (list "__NUMBER(context," value ")"))

                                         ((string? value)
                                          (list "__STRING(context," (quote-string value) ")"))

                                         ((symbol? value)
                                          (list "__SYMBOL(context," (quote-string (symbol->string value)) ")"))

                                         ((boolean? value)
                                          (list "__BOOLEAN_" (if value "TRUE" "FALSE")))

                                         ((pair? value)
                                          (list "__QUOTE(context," (quote-string (write-to-string value)) ")")
                                          )
                                         
                                         ((eq? value '())
                                          "__NULL")

                                         (else
                                          "__NULL")
                                         ))
                                       (l (vector-ref lambda-constant-vec offset))
                                       )
                                (if (pair? l)
                                    (vector-set! lambda-constant-vec offset
                                                 (cons (cons (+ 1 (car (car l))) cexp)
                                                       l))
                                    (vector-set! lambda-constant-vec offset
                                                 (cons (cons 0 cexp) l)))
                                (list "constant_" (car (car (vector-ref lambda-constant-vec offset)))))
                              ) (cdr exp)))
                    
                    (else
                     (begin
                       (display "!!!!") (display exp) (newline)
                       exp))
                    )
                   ))

    (c2c-context-lambda-start-set! c2c-context (+ next-offset lambda-start))
    (c2c-context-dump-set!
     c2c-context
     (let after-recur ((count (vector-length lambda-vec))
                       (declare-result '())
                       (define-result '()))
       (if (> count 0)
           (after-recur (- count 1)
                        (cons (list "static void lambda_" (+ count lambda-start -1) "(__context_t context);\n")
                              declare-result)
                        (cons (list "static void lambda_" (+ count lambda-start -1) "(__context_t context) {\n"
                                    ;; process for constant
                                    (let inner-recur ((clist (vector-ref lambda-constant-vec (+ count -1)))
                                                      (result '())
                                                      )
                                      (if (pair? clist)
                                          (inner-recur (cdr clist)
                                                       (cons

                                                        (list "static object_t constant_" (car (car clist)) " = NULL;\n"
                                                              "if (constant_" (car (car clist)) " == NULL) { constant_" (car (car clist))
                                                              " = " (cdr (car clist)) ";}\n")

                                                        result))

                                          result))
                                    
                                    (vector-ref lambda-vec (- count 1)) ";\n}\n")
                              define-result)
                        )
           (serialize-to-string
            (cons (list declare-result define-result
                        "void " main-name "(__context_t context) { lambda_" lambda-start "(context); }"
                        ) (c2c-context-dump c2c-context))))))
    ))

;; c2c.scm ends here.
