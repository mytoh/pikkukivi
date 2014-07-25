
(define-library (pikkukivi command verkko sssh)
    (export
      sssh)
  (import
    (scheme base)
    (gauche base)
    (gauche process)
    (util list) ; slices
    (file util)
    (srfi 1)
    (srfi  13))

  (begin
    (define (sssh args)
      (run-process `(ssh ,@args) :wait #true))))
