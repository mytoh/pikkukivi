
(define-library (pikkukivi commands verkko buk)
    (export buk)
  (import
    (scheme base)
    (gauche base)
    (pikkukivi commands verkko buk ningen-ng)
    (pikkukivi commands verkko buk ningen-ok)
    (util match))

  (begin
    (define (buk args)
      (match (car args)
             ("ningen-ng"
              (ningen-ng (cdr args)))
             ("ningen-ok"
              (ningen-ok (cdr args)))))
    ))
