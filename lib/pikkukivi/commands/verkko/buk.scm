
(define-module pikkukivi.commands.verkko.buk
  (export buk)
  (use pikkukivi.commands.verkko.buk.ningen-ng)
  (use pikkukivi.commands.verkko.buk.ningen-ok)
  (use util.match)
  )
(select-module pikkukivi.commands.verkko.buk)

(define (buk args)
  (match (car args)
    ("ningen-ng"
     (ningen-ng (cdr args)))
    ("ningen-ok"
     (ningen-ok (cdr args)))
    ))
