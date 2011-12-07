;; misc.scm

(define (quote-string string)

  (let ((length (string-length string)))
    (let recur ((current 0)
                (result '(#\")))

      (if (< current length)
          (let ((char (string-ref string current)))
            
            (if (eq? char #\")
                (recur (+ current 1)
                       (cons char (cons #\\  result)))
                (recur (+ current 1)
                       (cons char result))
                ))
          (list->string (reverse (cons #\" result)))
          )))
  )

(define (encode-to-legacy-token string)
  (let ((length (string-length string)))
    (let recur ((current 0)
                (result '()))

      (if (< current length)
          (let ((char (string-ref string current)))
            
            (if (or (char-alphabetic? char)
                    (char-numeric? char))
                (recur (+ current 1)
                       (cons char result))
                (recur (+ current 1)
                       (cons #\_ result))
                ))
          (list->string (reverse result))
          )))
  )

(define (write-to-string object)
  (cond
   ((symbol? object) (symbol->string object))
   ((number? object) (number->string object))
   ;; XXX incorrect!
   ((char? object)   (number->string (char->integer object)))
   ((boolean? object) (if object "#t" "#f"))
   ((vector? object)
    (string-append "#( "
                   (let recur ((count (vector-length object))
                               (result ""))
                     (if (> count 0)
                         (recur (- count 1)
                                (string-append (write-to-string (vector-ref object (- count 1))) " " result))
                         result)
                     )
                   ")"))
   
   ((pair? object)
    (string-append "("
                   (let recur ((cur object)
                               (result ""))
                     (if (pair? cur)
                         (recur (cdr cur)
                                (string-append result " " (write-to-string (car cur))))
                         (if (eq? cur '())
                             result
                             (string-append result " . " (write-to-string cur)))))
                   " )"))
   ((eq? object '()) "()")
   )
  )

(define make-fixed-set
  (lambda (elements)

    (let ((ele-list elements))
      
      (lambda (sym) (if (memq sym ele-list) #t #f))
      )))

;; misc.scm ends here.
