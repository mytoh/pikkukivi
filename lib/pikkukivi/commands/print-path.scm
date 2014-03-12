
(define-library (pikkukivi commands print-path)
    (export
      print-path)
  (import
    (scheme base)
    (scheme write)
    (gauche base)
    (gauche process)
    (util list) ; slices
    (util match)
    (file util)
    (gauche sequence)
    (kirjasto tiedosto))

  (begin

    (define (print-path args)
      (match args
             ((env)
              (print env)
              (cond
                ((sys-getenv env)
                 (for-each print
                   (string-split (sys-getenv env)
                                 ":")))
                (else env)))
             (_
              (for-each print
                (string-split (sys-getenv "PATH") ":")))))))
