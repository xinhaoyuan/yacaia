;; inline.scm

(let recur
    ((symbols '(cons car cdr set-car set-cdr
                    vector-ref vector-set!
                    not equal? eq?
                    + - > <
                    pair? vector?
                    ))
     (current 0)
     )
  
  (if (not (eq? symbols '()))

      (begin

        (add-macro! system-envir (car symbols) '(exp)
                    '(exp ...)
                    (list ysc-inline-apply current (list '@expand 'exp))
                    )      
        (recur (cdr symbols) (+ current 1))

        )))

;; inline.scm ends here.
