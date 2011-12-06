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
(load "base.scm")
(load "base-macros.scm")
(load "inline.scm")
(load "cps.scm")
(load "c2c.scm")

(define (get-input)

  ;; (newline)(display "ysc> ")
  (read)
  
  )

(define c2c-repl
  (lambda ()
    (let recur ((input-data (get-input))
                (context (make-c2c-context))
                )
      (if (not (eof-object? input-data))
          (let* ((cps-data (cps-with-system-envir
                            input-data))
                 ;; (output-data (c2c context "eval" cps-data))
                 )
            
            (display "** Compiling --- ") (display input-data) (newline)
            (display "** CPS --------- ") (display cps-data) (newline)
            ;; (display "** Result ------ ") (newline)
            ;; (display (c2c-context-dump context)) (newline)
            (recur (get-input)
                   (make-c2c-context)
                   )))
      )
    ))

(define c2c-file
  (lambda (context
           input-file-name
           output-file-name)

    (let ((input-port (open-input-file  input-file-name))
          (output-port (open-output-file output-file-name)))
      
      (let recur ((input-data (read input-port))
                  (input '()))

        (if (eof-object? input-data)
            (let ((cps (cps-with-system-envir (cons ysc-begin input))))
              ;; (display "CPS:") (display cps) (newline)
              (c2c context (encode-to-legacy-token input-file-name) cps)
              (display (string-append c2c-head-string (c2c-context-dump context) c2c-tail-string) output-port))
            (recur (read input-port) (cons input-data input))
            )))
    ))

(define c2c-files
  (lambda (file-name-list)
    (let recur ((context (make-c2c-context))
                (cur file-name-list))
      (if (pair? cur)
          (begin
            (c2c-file context
                      (car (car file-name-list))
                      (cdr (car file-name-list)))
            (c2c-context-dump-set! context '())
            (recur context
                   (cdr cur)))))))

;; ysc.scm ends here.
