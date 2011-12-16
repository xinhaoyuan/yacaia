;; context.scm
;;
;; Dynamic context support for the compiler

;; like a pair
(define make-context
  (lambda (mx-mode? envir)

    (let ((mx-mode? mx-mode?)
          (envir envir))

      (lambda (proc)
        (proc mx-mode? envir))

      )))

(define (make-exp-with-context context exp)
  (vector context exp))

(define (get-context exp) (vector-ref exp 0))
(define (get-expression exp) (vector-ref exp 1))

;; pass the context to proc with certain situation considered
(define call-with-context
  (lambda (proc context exp . etc)

    (context
     (lambda (mx-mode? envir)
       (if (eq? exp '())
           (apply proc (make-context #f envir) '() etc)
           (if mx-mode?
               
               (let ((ctx (get-context exp))
                     (exp (get-expression exp)))
                 (cond
                  ;; null context in the exp
                  ((eq? ctx '())
                   (apply proc context exp etc))
                  ;; only ``mx-mode?'' stored
                  ((or (eq? ctx #f) (eq? ctx #t))
                   (apply proc (make-context
                                ctx
                                envir)
                          exp
                          etc))
                  ;; full
                  (else
                   (apply proc ctx exp etc))
                  ))
               
               (apply proc context exp etc))
           ))
     )
    ))

;; similar to call-with-context, with one more arg to indicate the
;; situation
(define call-with-free-context
  (lambda (proc context exp . etc)
    
    (context
     (lambda (mx-mode? envir)
       
       (if mx-mode?
           
           (let ((ctx (get-context exp))
                 (exp (get-expression exp)))
             
             (cond
              ((eq? ctx '())
               ;; null context in exp
               (apply proc #t context exp etc))
              ((or? (eq? ctx #f) (eq? ctx #t))
               ;; only ``mx-value'' in exp
               (apply proc #t (make-context
                               ctx
                               envir)
                      exp
                      etc))
              (else
               ;; full
               (apply proc #f ctx exp etc)
               )
              ))
           (apply proc #t context exp etc))
       ))
    ))

(define context-lookup-binding
  (lambda (context sym)
    (context
     (lambda (mx-mode? envir)
       (envir-lookup-binding envir sym)))
    ))

(define context-lookup-ruledef
  (lambda (context sym)
    (context
     (lambda (mx-mode? envir)
       (envir-lookup-ruledef envir sym)))
    ))

(define context-lookup-typedef
  (lambda (context sym)
    (context
     (lambda (mx-mode? envir)
       (envir-lookup-typedef envir sym)))
    ))

(define (context-lookup context sym)
  (if (symbol? sym)
      (context
       (lambda (mx-mode? envir)
         (envir-lookup envir sym)
         ))
      '())
  )

(define purge
  (lambda (context element)
    (call-with-context (lambda (context exp)
                         exp) context element)
    ))

;; ============================================================

(define purge-plist
  (lambda (context list)

    (let ((result (deque)))
      (let recur ((current list))
        
        (if (not (pair? current))
            (begin
              (result 'set-tail! (purge context current))
              (result 'plist))
            (begin
              (result 'push-tail!
                      (purge context (car current)))
              (recur (cdr current))))
        ))
    ))

(define purge-exp
  (lambda (context exp)

    (if (not (pair? exp))
        
        exp
        
        (let ((result (deque)))
          (let recur ((current exp))
            
            (if (not (pair? current))
                (begin
                  (result 'set-tail! (call-with-context
                                      purge-exp context current))
                  (result 'plist))
                (begin
                  (result
                   'push-tail!
                   (call-with-context
                    purge-exp context (car current)))
                  (recur (cdr current))))
            ))
        )
    ))

;; context.scm ends here.
