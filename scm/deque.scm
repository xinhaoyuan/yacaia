;; deque.scm

(define make-deque
  (lambda (init-size
           init-head
           init-tail)

    (letrec ((size init-size)
             (head init-head)
             (tail init-tail)
             
             (get-head (lambda () (car head)))
             (plist (lambda () head))

             (empty? (lambda ()
                       (eq? size 0)))

             (append-head
              (lambda (element)

                (let ((new-head (cons element head)))
                  (make-deque (+ size 1)
                              new-head
                              (if (eq? tail '()) new-head tail))
                  )))
             
             (push-head!
              (lambda (element)

                (set! size (+ size 1))
                (set! head (cons element
                                 head))
                (and (eq? tail '()) (set! tail head))
                ))

             (push-tail!
              (lambda (data)
                (set! size (+ size 1))
                (if (eq? tail '())
                    (begin
                      (set! tail (cons data '()))
                      (set! head tail))
                    (begin
                      (set-cdr! tail (cons data '()))
                      (set! tail (cdr tail))))
                ))

             (set-tail!
              (lambda (data)
                (if (eq? tail '())
                    (set! head data)
                    (set-cdr! tail data))))

             (set-head! (lambda (data) (set-car! head data)))
             (pop-head! (lambda ()
                          (set! size (- size 1))
                          (set! head (cdr head))
                          (and (eq? head '()) (set! tail '()))
                          ))

             (merge-tail! (lambda (deque)
                            (deque 'dump
                                   (lambda (obj-size obj-head obj-tail)
                                     
                                     (set! size (+ size obj-size))
                                     (if (eq? head '())
                                         (set! head obj-head)
                                         (set-cdr! tail obj-head))
                                     (if (not (eq? obj-tail '()))
                                         (set! tail obj-tail))))
                            ))
             )

      (lambda (op . args)
        (cond

         ((eq? op 'dump) (apply
                          (lambda (proc)
                            (proc size head tail))
                          args))

         ((eq? op 'head) (apply get-head args))
         ((eq? op 'plist) (apply plist args))
         ((eq? op 'size) size)
         ((eq? op 'empty?) (apply empty? args))
         ((eq? op 'append-head) (apply append-head args))
         ((eq? op 'push-head!) (for-each push-head! args))
         ((eq? op 'push-tail!) (for-each push-tail! args))
         ;; danger operations
         ((eq? op 'set-tail!) (apply set-tail! args))
         ((eq? op 'set-head!) (apply set-head! args))
         ((eq? op 'pop-head!) (apply pop-head! args))
         ((eq? op 'merge-tail!) (apply merge-tail! args))

         (else (display "deque: unknown operation. - ") (display op) (newline))
         
         )))
    ))

(define (plist->deque init-plist)
  (if (pair? init-plist)

      (let recur ((size 1)
                  (current init-plist))

        (if (not (pair? (cdr current)))

            (make-deque size init-plist current)
            (recur (+ size 1) (cdr current))
            ))
      
      (make-deque 0 '() '())
      
      ))

(define (deque . init-plist)
  (plist->deque init-plist))

;; code for test

;; (define foo (deque 0 -1 -2))
;; (foo 'push-head! 1 2 3)

;; (display (foo 'plist))(newline)
;; (foo 'push-tail! 4 5 6)
;; (display (foo 'plist))(newline)

;; (let recur ()
;;   (if (not (foo 'empty?))
;;       (begin
;;        	(display (foo 'head)) (display " ")
;;  	(foo 'pop-head!)
;;  	(recur))))

;; (define bar (deque 3 6 9))
;; (foo 'merge-tail! bar)

;; (display (foo 'plist)) (newline)

;; deque.scm ends here.
