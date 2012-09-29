
(define-module pikkukivi.cli
  (export runner)
  (use gauche.process)
  (use file.util)
  (use util.match)
  (use util.list)
  (use pikkukivi.alias)
  (extend
    pikkukivi.commands))
(select-module pikkukivi.cli)


(define (screen-title command)
  (cond
    ((equal?  (sys-basename (sys-getenv "SHELL"))
              "tcsh")
     (display (string-append "_" command "")))
     (else
       (display  (string-append "k" command  "\\"))))) 

(define (run-alias command args)
  (let* ((c (assoc-ref alias-list (string->symbol command)))
         (cmd (if c (car c) #f)))
    (cond
      ((string? cmd)
       (screen-title command)
       (run-process `(,@(string-split cmd " ") ,@args) :wait #t))
      ((procedure? cmd)
       (screen-title command)
       (cmd args))
      (else
        ((eval (read-from-string command) 'user) args)))))

(define (runner args)
  (run-alias (car args) (cdr args))
  0)
