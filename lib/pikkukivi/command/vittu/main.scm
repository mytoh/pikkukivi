;;; vittu.scm

 (define-library (pikkukivi command vittu main)
    (export vittu)
  (import (scheme base)
          (scheme write)
          (srfi 13)
          (gauche base)
          (gauche process)
          (kirjasto komento)
          (kirjasto merkkijono))

  (begin

    (define (search-process prog)
      (let ((result (process-output->string `(pgrep ,prog)
                                            :on-abnormal-exit :ignore)))
        (not (string-null? result))))

    (define (kill-process prog)
      (run-command `(killall -9 ,prog)))

    (define (process-not-found prog)
      (println (string-append prog " not found")))

    (define (message-killed prog)
      (let* ((result (process-output->string `(toilet -f term -F rotate ,prog)))
             (message (string-append " (╯°□°）╯︵ " result)))
        (newline)
        (println message)))

    (define (kill-processes procs)
      (for-each
          (lambda (proc)
            (cond
              ((search-process proc)
               (kill-process proc)
               (message-killed proc))
              (else (process-not-found proc))))
        procs))

    (define (vittu args)
      (kill-processes args))

    ))
