;; hygienic-rule.scm

(define (read-hygienic-rule

         context
         input

         label-size
         input-rule
         )

  ;; (display input-rule)(newline)
  (call-with-current-continuation
   (lambda (cont)
     (letrec ((label-vec (make-vector label-size))

              (label-set-tail!
               (lambda (id val)
                 ((vector-ref label-vec id)
                  'set-tail! val)))
              
              (label-write!
               (lambda (id val)
                 ((vector-ref label-vec id)
                  'write! val)))
              
              (label-top-level?
               (lambda (id)
                 ((vector-ref label-vec id)
                  'top-level?)))
              
              (label-push!
               (lambda (id)
                 ((vector-ref label-vec id)
                  'push-level!)))
              
              (label-pop!
               (lambda (id)
                 ((vector-ref label-vec id)
                  'pop-level!)))
              
              (current-level 0)
              (rolling-level 0)
              (low-level 0)

              (inc-level
               (lambda ()
                 (set! current-level (+ current-level 1))))
              
              (dec-level
               (lambda ()
                 (set! current-level (- current-level 1))
                 (set! low-level current-level)
                 ))
              )

       ;;----------------------------------------

       ;; initialize the label table
       (let recur ((idx 0))
         (and (< idx label-size)
              (begin
                (vector-set! label-vec idx (make-plist-writer))
                (recur (+ idx 1)))))
       
       ;; read the input rule and the input list
       (let read-rule ((context context)
                       (exp input)
                       (rule input-rule)
                       (tail?  #f))

         (cond

          ((eq? (car rule) tag-label)
           (let ((id (car (cdr rule))))
             
             (and (not (label-top-level? id))
                  (let recur ((iter low-level))
                    (and (< iter current-level)
                         (begin
                           (label-pop id)
                           (recur (+ iter 1))))))
             
             (let recur ((iter (or low-level 0)))
               (and (< iter current-level)
                    (begin
                      (label-push! id)
                      (recur (+ iter 1)))))
             
             (if tail?
                 (or (eq? exp '())
                     (label-set-tail! id (make-exp-with-context context exp)))
                 (label-write! id (make-exp-with-context context exp)))
             #t))
          
          ((eq? (car rule) tag-constant)
           ;; purge the expression and compare
           (if (equal? (purge-exp context exp)
                       (car (cdr rule)))
               #t
               (cont #f)))

          ;; recursive matching
          ((eq? (car rule) tag-list)
           (let recur ((current-rule (cdr rule))
                       (current-exp exp))

             (if (eq? (cdr current-rule) '())
                 ;; tail-pattern
                 (if (eq? (car current-rule) '())
                     (if (not (eq? current-exp '()))
                         (cont #f)
                         #t)
                     (begin

                       (inc-level)

                       (let recur ((current current-exp))
                         
                         (if (pair? current)
                             
                             (let ((tmp
                                    (call-with-context read-rule
                                                       context
                                                       (car current)
                                                       (car current-rule)
                                                       #f)
                                    ))
                               
                               (set! low-level current-level)
                               (and tmp (recur (cdr current))))

                             (let ((tmp
                                    (call-with-context read-rule
                                                       context
                                                       current
                                                       (car current-rule)
                                                       #t)
                                    ))
                               
                               (set! low-level current-level)
                               (dec-level)
                               tmp)
                             )
                         )
                       ))

                 (if (not (pair? current-exp))
                     (if (and tail?
                              (eq? current-exp '()))
                         #t (cont #f))
                     (and
                      (call-with-context read-rule
                                         context
                                         (car current-exp)
                                         (car current-rule)
                                         #f
                                         )
                      (recur (cdr current-rule) (cdr current-exp))))
                 
                 )
             ))
          ))
       
       ;; convert
       (let recur ((id 0))
         (and (< id label-size)
              (begin
                (vector-set! label-vec id
                             ((vector-ref label-vec id) 'finish!))
                (recur (+ id 1)))))

       ;; return value
       label-vec
       ))))

(define (debug-hygienic-rule
         label-vec label-size)

  (let recur ((id 0))

    (and (< id label-size)
         (begin
           (display id)(display " -- ")(display (vector-ref label-vec id))(newline)
           (recur (+ id 1))))
    )

  (display " -- Finish.")(newline)
  )

(define (write-hygienic-rule
         
         label-vec
         
         label-size
         output-rule)

  ;; (debug-hygienic-rule
  ;;  label-vec label-size)
  ;; convert
  (let recur ((id 0))
    (and (< id label-size)
         (begin
           (vector-set! label-vec id
                        (make-plist-reader (vector-ref label-vec id)))
           (recur (+ id 1)))
         ))

  (call-with-current-continuation
   (lambda (cont)

     (letrec

         ((label-read
           (lambda (id)
             ((vector-ref label-vec id)
              'read)))
          (label-tail?
           (lambda (id)
             ((vector-ref label-vec id)
              'tail?)))

          (label-roll!
           (lambda (id)
             ((vector-ref label-vec id)
              'next!)))
          
          (label-top-level?
           (lambda (id)
             ((vector-ref label-vec id)
              'top-level?)))
              
          (label-push!
           (lambda (id)
             ((vector-ref label-vec id)
              'push-level!)))
          
          (label-pop!
           (lambda (id)
             ((vector-ref label-vec id)
              'pop-level!)))
          
          (output (make-plist-writer))
          
          (label-mark-vec
           (make-vector label-size 0))
          
          (current-mark-id 0)
          
          (make-mark
           (lambda (level)
                 (let ((mark-id current-mark-id))
                   (set! current-mark-id
                         (+ current-mark-id 1))
                   (cons level mark-id))))
          
          (mark-level car)
          (mark-id    cdr)
          
          (label-error-vec
           (make-vector label-size #f))
          
          (label-get-error
           (lambda (id)
             (vector-ref label-error-vec id)))
          
          (label-set-error!
           (lambda (id level)
             (vector-set! label-error-vec id level)))
          
          (label-update-error!
           (lambda (id level)
             (if (eq? (label-get-error id) level) #f
                 (begin
                   (label-set-error! id #f)
                   #t))))
          
          (label-update-mark!
           (lambda (id mark-id)
             (if (> mark-id (vector-ref label-mark-vec id))
                 (begin
                   (vector-set! label-mark-vec id mark-id)
                   #t)
                 #f)))
          )
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

       (let write-recur ((roll-mark (make-mark 0))
                         (current-level 0)
                         (output output)
                         (rule output-rule))

         ;; (trace write-recur)
         ;; (trace output)
         ;; (trace label-push!)
         ;; (trace label-read)
         ;; (trace label-roll!)
         
         (cond
          
          ;; output constant
          ((eq? (car rule) tag-constant)
           (output 'write! (car (cdr rule)))
           '())

          ;; output for label
          ((eq? (car rule) tag-label)
           (apply
            (lambda (native? id)

              (and (and
                    (not (label-top-level? id))
                    ;; ??? I forget the funcationality -- magic
                    (label-update-error! id (mark-level roll-mark)))
                   
                   (begin
                     (let recur ((iter (mark-level roll-mark)))
                       (and (< iter current-level)
                            (begin
                              (label-pop! id)
                              (recur (+ iter 1)))))
                     (and (label-update-mark! id (mark-id roll-mark))
                          (label-roll! id)
                          ))
                   ;; 		  (begin
                   ;; 		    (display (label-get-error id)) (display " :: ") (display (mark-level roll-mark))
                   ;; 		    (newline))
                   )
              
              ;; mark the level where failure occurs
              (let recur ((iter (mark-level roll-mark)))
                (if (< iter current-level)
                    (begin
                      (label-push! id)
                      (recur (+ iter 1)))

                    (if (eq? (label-read id) '())
                        
                        (let ((error-level (mark-level roll-mark)))
                          (label-set-error! id error-level)
                          (if (eq? error-level 0) '() error-level))
                        
                        (let ((context (get-context (label-read id)))
                              (exp (get-expression (label-read id))))
                          
                          ;; (display " -- : ")(display (label-read id))(newline)
                          
                          (if native?
                              (output (if (label-tail? id) 'set-tail! 'write!)
                                      (vector
                                       (context (lambda (mx-mode? envir) mx-mode?))
                                       exp)
                                      )
                              
                              (output (if (label-tail? id) 'set-tail! 'write!)
                                      (label-read id))
                              )
                          
                          (if (label-tail? id)
                              ;; (let ((error-level (mark-level roll-mark)))
                              ;;   (display "!!!!!!!!!!") (display roll-mark) (newline)
                              ;;   (label-set-error! id error-level)
                              ;;   (if (eq? error-level 0) '() error-level)
                              ;;   )
                              '()
                              #f)))
                    ))

              ) (cdr rule))
           )
          
          ((eq? (car rule) tag-hide)
           ;; redirect the output to a dummy plist
           (write-recur roll-mark
                        current-level
                        (make-plist-writer)
                        (car (cdr rule)))
           )
          
          ;; recur output in a list
          ((eq? (car rule) tag-list)
           (output 'push-level!)
           
           (let ((result '()))
             (let recur ((current (cdr rule)))
               (if (not (eq? current '()))
                   (let ((current-result
                          (write-recur roll-mark current-level
                                       output (car current))))
                     (if (eq? current-result '())
                         (recur (cdr current))
                         (if (or (eq? result '())
                                 (eq? result current-result))
                             (begin
                               (set! result current-result)
                               (recur (cdr current)))
                             ;; ??? chinese comment
                             ;; 结果不一致直接导致失败
                             (cont #f))
                         ))
                   (begin
                     ;; use the default context
                     (output 'write!
                             (make-exp-with-context
                              '()
                              (output 'pop-back!)
                              ))
                     result)
                   ))
             )

           )

          ;; recur output with expanding
          ((eq? (car rule) tag-expand)
           (let ((expand-result #f))

             (set! current-level (+ current-level 1))
             (let repeat ()

               (output 'push-level!)
               (let ((result '()))
                 
                 (let recur ((current (cdr rule)))
                   
                   (if (not (eq? current '()))
                       (let ((current-result
                              (write-recur roll-mark current-level
                                           output (car current))))
                         (if (eq? current-result '())
                             (recur (cdr current))
                             (if (or (eq? result '())
                                     (eq? result current-result))
                                 (begin
                                   (set! result current-result)
                                   (recur (cdr current)))
                                 ;; ??? chinese comment
                                 ;; 同样, 结果不一致导致失败
                                 (cont #f))))

                       (begin
                         (if (or (eq? result '())
                                 (not result))
                             (begin
                               (output 'pop-level-expand!)
                               (set! roll-mark (make-mark current-level))
                               )

                             (output 'pop-back!))
                         
                         (if (not result)
                             (repeat)
                             (if (eq? result current-level)
                                 #f result)
                             )
                         )
                       ))))
             ))
          
          ))

       (let ((result (car (output 'finish!))))
         (make-exp-with-context
          #t
          (get-expression result)
          ))
       )
     ))
  )

;; hygienic-rule.scm ends here.
