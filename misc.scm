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

(define make-fixed-set
  (lambda (elements)

    (let ((ele-list elements))
      
      (lambda (sym) (if (memq sym ele-list) #t #f))
      )))

;; misc.scm ends here.
