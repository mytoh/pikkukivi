
(define-library (pikkukivi command yes)
    (export
      yes)
  (import
    (scheme base)
    (gauche base)
    (util match))

  (begin
    (define (yes args)
      (match args
             ((text)
              (let loop ()
                   (print text)
                   (loop)))
             (()
              (let loop ()
                   (print "y")
                   (loop)))))
    ))
