
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
    (file util)
    (kirjasto merkkijono))

  (begin

    (define (print-path args)
      (match args
             ((env)
              (println env)
              (cond
                ((get-environment-variable env)
                 (for-each println
                   (string-split (get-environment-variable env)
                                 ":")))
                (else env)))
             (_
              (for-each println
                (string-split (get-environment-variable"PATH") ":")))))))
