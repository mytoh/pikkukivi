;;; vittu.scm

 (define-library (pikkukivi command vittu)
    (export vittu)
  (import (scheme base)
          (scheme write)
          (srfi 13)
          (gauche base)
          (gauche process)
          (kirjasto komento))

  (begin

    (define (search-process prog)
      (let ((result (process-output->string `(pgrep ,prog)
                                            :on-abnormal-exit :ignore)))
        (not (string-null? result))))

    (define (kill-process prog)
      (run-command `(killall ,prog)))

    (define (process-not-found prog)
      (display (string-append prog " not found"))
      (newline))

    (define (message-killed prog)
      (let* ((result (process-output->string `(toilet -f term -F rotate ,prog)))
             (message (string-append " (╯°□°）╯︵ " result)))
        (newline)
        (display message)
        (newline)))

    (define (vittu args)
      (cond
        ((search-process (car args))
         (kill-process (car args))
         (message-killed (car args)))
        (else (process-not-found (car args)))))

    ))
