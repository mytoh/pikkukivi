
(define-library (pikkukivi cli)
    (export run)
  (import (scheme base)
          (scheme repl)
          (scheme eval)
          (scheme r5rs)
          (gauche base)
          (file util)
          (util list)
          (pikkukivi command))
  (begin

    (define (run args)
      (let ((command (cadr args))
            (rest (cddr args)))
        ((string->command command) rest)))

    ))
