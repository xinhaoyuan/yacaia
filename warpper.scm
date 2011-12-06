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
(load "compile.scm")

(define (get-input)

  ;; (newline)(display "ysc> ")
  (read)
  
  )

(define repl
  (lambda ()
    (let recur ((input-data (get-input))
                (context (make-compile-context))
                )
      (if (not (eof-object? input-data))
          (let* ((cps-data (cps-with-system-envir
                            input-data))
                 (output-data (compile context cps-data)))
            
            (display "** Compiling --- ") (display input-data) (newline)
            (display "** CPS --------- ") (display cps-data) (newline)
            (display "** Result ------ ") (newline)
            (display (cc-dump context)) (newline)
            ;; (display "   Executing ... ") (newline)
            ;; (test-vm output-data)
            (recur (get-input)
                   (make-compile-context)
                   )))
      )
    ))

(define compile-file
  (lambda (context
           input-file-name
           output-file-name)

    (let ((input-port (open-input-file  input-file-name))
          (output-port (open-output-file output-file-name)))
      
      (let recur ((input-data (read input-port))
                  (input '()))

        (if (eof-object? input-data)
            (begin
              (compile context (encode-to-legacy-token input-file-name) (cps-with-system-envir (cons ysc-begin input)))
              (display (string-append compile-head-string (cc-dump context) compile-tail-string) output-port))
            (recur (read input-port) (cons input-data input))
            )))
    ))

(define compile-files
  (lambda (file-name-list)
    (let recur ((context (make-compile-context))
                (cur file-name-list))
      (if (pair? cur)
          (begin
            (compile-file context
                          (car (car file-name-list))
                          (cdr (car file-name-list)))
            (cc-dump-set! context '())
            (recur context
                   (cdr cur)))))))

;; ysc.scm ends here.
