;; error.scm

(define (compiling-log) (deque))

(define (push-comp-log!
	 comp-log
	 comp-msg
	 dl-position)

  (comp-log 'push-head! (cons comp-msg dl-position)))

(define (pop-comp-log! comp-log)
  (comp-log 'pop-head!))

;; error.scm ends here.
