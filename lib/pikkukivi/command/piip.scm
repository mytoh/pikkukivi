
(define-library (pikkukivi command piip)
    (export
      piip)
  (import
    (scheme base)
    (scheme file)
    (gauche base))

  (begin
    (define (piip args)
      (cond
        ((equal? (sys-getenv "OSTYPE")
           "FreeBSD")
         (call-with-output-file
             "/dev/speaker"
           (lambda (in)
             (display (car args) in))))
        (else
            (display ""))))))
