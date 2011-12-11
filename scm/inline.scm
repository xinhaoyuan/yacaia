;; inline.scm
;;
;; defines some calls that can be inlined to speed up the execution

(let recur
    ((symbols (quote
               ((car         . 0)
                (cdr         . 1)
                (set-car     . 2)
                (set-cdr     . 3)
                (vector-ref  . 4)
                (vector-set! . 5)
                (not         . 6)
                (equal?      . 7)
                (eq?         . 8)
                (+           . 9)
                (-           . 10)
                (>           . 11)
                (<           . 12)
                (pair?       . 13)
                (vector?     . 14)))
               ))
  
  (if (not (eq? symbols '()))

      (begin

        (add-macro! system-envir (car (car symbols)) '(exp)
                    '(exp ...)
                    (list ysc-inline-apply (cdr (car symbols)) (list '@expand 'exp))
                    )      
        (recur (cdr symbols))
        )))

;; inline.scm ends here.
