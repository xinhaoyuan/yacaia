;; c2c.scm
;;
;; convert CPS into C snippets

(define c2c-head-string
  "\
#include <config.h>
#include <headers.h>
")
(define c2c-tail-string "")

(define (make-c2c-context)
  (cons 0 ""))

(define c2c-context-dump cdr)
(define c2c-context-dump-set! set-cdr!)

(define c2c-context-lambda-start car)
(define c2c-context-lambda-start-set! set-car!)

(define (quote-exp value)
  (cond
   ((integer? value)
    (list "INTEGER(context," value ")"))
   
   ((number? value)
    (list "NUMBER(context," value ")"))

   ((string? value)
    (list "STRING(context," (quote-string value) ")"))

   ((symbol? value)
    (list "SYMBOL(context," (quote-string (symbol->string value)) ")"))

   ((boolean? value)
    (list "BOOLEAN_" (if value "TRUE" "FALSE") "(context)"))

   ((pair? value)
    (let recur ((count 0)
                (cur value)
                (result '())
                )
      (if (pair? cur)
          (recur (+ 1 count)
                 (cdr cur)
                 (cons (quote-exp (car cur))
                       (cons "," result)))
          (cons "LCONS(context,"
                (cons count
                      (reverse
                       (cons ")" (cons (quote-exp cur) (cons "," result)))
                       )))
          )))

   ((vector? value)
    (let recur ((idx (vector-length value))
                (result '(")")))
      (if (eq? idx 0)
          (cons (list "VECTOR(context," (vector-length value)) result)
          (begin
            (set! idx (- idx 1))
            (recur idx
                   (cons ","
                         (cons (quote-exp (vector-ref value idx)) result)))
            ))))

   ((char? value)
    (list "INTEGER(context," (char->integer value) ")"))
   
   ((eq? value '())
    "OBJECT_NULL")

   (else
    (begin (display "unknown exp to represent") (display value) (newline)))
  ))

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
                                (list "CLOSURE(context,lambda_" id "," argc ")")
                                ))
                            
                            (cdr exp)))

                    ((eq? (car exp) ysc-set!)
                     (apply (lambda (ref value)

                              (set! value (recur value))
                              
                              (if (pair? ref)
                                  (list "SET_LOCAL(context," (car ref) "," (cdr ref) "," value ")")
                                  (list "SET_GLOBAL(context," (quote-string (symbol->string ref)) "," value ")")
                                  )

                              ) (cdr exp)))

                    ((or (eq? (car exp) ysc-apply)
                         (eq? (car exp) ysc-apply-tl))
                     (let ((writer (make-plist-writer)))
                       (writer 'push-level!)

                       (cond
                        ((eq? (car exp) ysc-apply)
                         (writer 'write! "APPLY(context,"))
                        ((eq? (car exp) ysc-apply-tl)
                         (writer 'write! "APPLY_TL(context,")))

                       (writer 'write! (length (cdr exp)))

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
                                (writer 'write! "INLINE_" (car (cdr inline-no)) "(context," (length args))

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

                              (list "if (IS_TRUE(" (recur if-cond) ")) {"
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


                    ((eq? (car exp) 'exit) (list "EXIT(context," (recur (car (cdr exp))) ")"))
                    ((eq? (car exp) 'exit-proc) "EXIT_PROC")

                    ((eq? (car exp) ysc-get)
                     (apply (lambda (ref)

                              (if (pair? ref)
                                  (list "GET_LOCAL(context," (car ref) "," (cdr ref) ")")
                                  (list "GET_GLOBAL(context," (quote-string (symbol->string ref)) ")"))
                              
                              ) (cdr exp)))

                    ((eq? (car exp) ysc-quote)
                     (apply (lambda (value)
                              (letrec ((offset (car offset-stack))
                                       (l (vector-ref lambda-constant-vec offset))
                                       )
                                (if (pair? l)
                                    (vector-set! lambda-constant-vec offset
                                                 (cons (cons (+ 1 (car (car l))) (quote-exp value))
                                                       l))
                                    (vector-set! lambda-constant-vec offset
                                                 (cons (cons 0 (quote-exp value)) l)))
                                (list "constant_" (car (car (vector-ref lambda-constant-vec offset)))))
                              ) (cdr exp)))
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
                        (cons (list "static void lambda_" (+ count lambda-start -1) "(context_t context);\n")
                              declare-result)
                        (cons (list "static void lambda_" (+ count lambda-start -1) "(context_t context) {\n"
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
                        "void " main-name "(context_t context) { lambda_" lambda-start "(context); }"
                        ) (c2c-context-dump c2c-context))))))
    ))

;; c2c.scm ends here.
