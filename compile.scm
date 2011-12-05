;; compile.scm

(define compile-head-string
  "\
#include \"headers.h\"

void entry(__context_t context){
switch (context->closure_id) {
")
(define compile-tail-string "\
default: break;
}
}
")

(define (compile cps-exp)
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
           
           (lcount 1)

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

                              (let ((id lcount))
                                (set! lcount (+ 1 lcount))
                                (vector-set! lambda-vec id (recur body))
                                (list "create_lambda(context," id "," argc ")")
                                ))
                            
                            (cdr exp)))

                    ((eq? (car exp) ysc-set-cps)
                     (apply (lambda (cont ref value)

                              (set! cont (recur cont))
                              (set! value (recur value))
                              
                              (if (pair? ref)
                                  (list "set_local(context," cont "," (car ref) "," (cdr ref) "," value ")")
                                  (list "set_global(context," cont "," (quote-string (symbol->string ref)) "," value ")")
                                  )

                              ) (cdr exp)))

                    ((or (eq? (car exp) ysc-apply)
                         (eq? (car exp) ysc-apply-tl)
                         (eq? (car exp) ysc-inline-apply))
                     (let ((writer (make-plist-writer)))
                       (writer 'push-level!)

                       (cond
                        ((eq? (car exp) ysc-apply)
                         (writer 'write! "apply(context"))
                        ((eq? (car exp) ysc-apply-tl)
                         (writer 'write! "apply_tl(context"))
                        ((eq? (car exp) ysc-inline-apply)
                         (writer 'write! "apply_inline(context")))

                       (let inner-recur ((cur (cdr exp)))

                         (if (pair? cur)
                             (begin
                               (writer 'write! ",")
                               (writer 'write! (recur (car cur)))
                               (inner-recur (cdr cur)))))
                       (writer 'write! ")")
                       (car (writer 'finish!))
                       ))

                    ((eq? (car exp) ysc-if)
                     (apply (lambda (if-cond if-true if-false)

                              (list "if (is_ture(" (recur if-cond) ")) {"
                                    (recur if-true) "} else {" (recur if-false) "}")

                              ) (cdr exp)))

                    ((eq? (car exp) 'exit) exp)

                    ((eq? (car exp) ysc-get)
                     (apply (lambda (ref)

                              (if (pair? ref)
                                  (list "get_local(context," (car ref) "," (cdr ref) ")")
                                  (list "get_global(context," ref ")"))
                              
                              ) (cdr exp)))

                    ((eq? (car exp) ysc-quote)
                     (apply (lambda (value)

                              (cond
                               ((number? value)
                                (list "__NUMBER(" value ")"))

                               ((string? value)
                                (list "__STRING(" (quite-string value) ")"))

                               ((symbol? value)
                                (list "__SYMBOL(" (quite-string value) ")"))
                               )
                              )(cdr exp)))

                    (else exp)
                    )
                   ))

    (let after-recur ((count (vector-length lambda-vec))
                      (result (list compile-tail-string)))
      (if (> count 0)
          (after-recur (- count 1)
                       (cons (list "case " (- count 1) ":\n" (vector-ref lambda-vec (- count 1)) ";break;\n")
                             result)
                       )
          (serialize-to-string (cons compile-head-string result))))
  ))

;; compile.scm ends here.
