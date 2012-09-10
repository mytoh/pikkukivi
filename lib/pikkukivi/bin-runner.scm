
(define-module pikkukivi.bin-runner
  (use gauche.process)
  (use file.util)
  (use util.match)
  (use util.list)
  (use pikkukivi.alias)
  (export bin-runner)
  (extend
    pikkukivi.commands))
(select-module pikkukivi.bin-runner)


(define (screen-title command)
  (cond
    ((equal?  (sys-basename (sys-getenv "SHELL"))
              "tcsh")
     (display (string-append "_" command "")))
     (else
       (display  string-append "k" command  "\\"))))

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

(define (bin-runner args)
  (run-alias (car args) (cdr args))
  0)
