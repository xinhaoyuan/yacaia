;; dplist.scm

(define (dpl-last dplist)  (car (cdr dplist)))
(define (dpl-next dplist)  (cdr (cdr dplist)))
(define (dpl-tail? dplist) (eq? (dpl-next dplist) '()))
(define (dpl-read dplist)  (car dplist))

(define (plist->dplist exp)
  (if (not (pair? exp)) exp
      (let ((result (cons (car exp)
			  (cons '()
				'()))
		    ))
	
	(let convert-recur
	    ((before result)
	     (current (cdr exp))
	     )
	
	(if (not (pair? current))

            (begin
              (set-cdr! (cdr before)
                        (cons current
                              (cons before
                                    '())))
              result
              )
            
	    (begin
	      (set-cdr! (cdr before)
			(cons (car current)
			      (cons before
				    '())))
	      (convert-recur (dpl-next before) (cdr current))
	      
	      ))))
      ))

;; dplist.scm ends here.
