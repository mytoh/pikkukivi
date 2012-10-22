


(define-module pikkukivi.commands.piip
  (export
    piip))
(select-module pikkukivi.commands.piip)

(define (piip args)
  (cond
    ((equal? (sys-getenv "OSTYPE")
             "FreeBSD")
     (call-with-output-file
       "/dev/speaker"
       (lambda (in)
         (display (car args) in))))
    (else
      (display ""))))