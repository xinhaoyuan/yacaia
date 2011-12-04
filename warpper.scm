;; warpper.scm

(load "deque.scm")
(load "error.scm")
(load "dplist.scm")
(load "envir.scm")
(load "plist-rw.scm")
(load "context.scm")
(load "misc.scm")
(load "rule-compile.scm")
(load "hygienic-rule.scm")
(load "system-rules.scm")
(load "base-macros.scm")
(load "inline.scm")
(load "compile-base.scm")
(load "compile.scm")
(load "cps.scm")

(define (get-input)

  ;; (newline)(display "ysc> ")
  (read)
  
  )

(define repl
  (lambda ()
    (let recur ((input-data (get-input)))
      (if (not (eof-object? input-data))
	  (let ((output-data
		 (cps-with-system-envir
		  input-data)))
	    (display "** Compiling --- ") (display input-data) (newline)
	    (display "** Result ------ ") 
	    (display output-data) (newline)
	    ;; (display "   Executing ... ") (newline)
	    ;; (test-vm output-data)
	    (recur (get-input))))
      )
    ))

(define compile-file
  (lambda (input-file-name
	   output-file-name)

    (let ((input-port  (open-input-file  input-file-name))
	  (output-port (open-output-file output-file-name)))
      
      (let recur ((input-data (read input-port)))

	(if (not (eof-object? input-data))
	    (begin
;;	      (display "Compiling: ") (display input-data) (newline)
	      (display (compile-with-system-envir input-data)
		       output-port) (newline output-port)
	      (recur (read input-port)))
	    )))
    ))

(define (compile file-name)
  (compile-file file-name (string-append file-name ".vm"))
  )

(repl)

;; ysc.scm ends here.
