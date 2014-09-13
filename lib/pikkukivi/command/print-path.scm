
(define-library (pikkukivi command print-path)
    (export
      print-path)
  (import
    (scheme base)
    (scheme write)
    (scheme process-context)
    (gauche base)
    (gauche process)
    (util list) ; slices
    (util match)
    (file util))

  (begin

    (define (print-path args)
      (match args
             ((env)
              (print env)
              (cond
                ((get-environment-variable env)
                 (for-each print
                   (string-split (get-environment-variable env)
                                 ":")))
                (else env)))
             (_
              (for-each print
                (string-split (get-environment-variable"PATH") ":")))))))
