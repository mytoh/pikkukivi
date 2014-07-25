
(define-library (pikkukivi command verkko)
    (export
      futaba
      yotsuba
      konachan
      buk
      pahvi
      radio
      sget
      ;; pervo
      ylilauta
      lainchan
      sssh)
  (import
    (scheme base)
    (pikkukivi command verkko futaba)
    (pikkukivi command verkko yotsuba)
    (pikkukivi command verkko konachan)
    (pikkukivi command verkko lainchan)
    (pikkukivi command verkko pahvi)
    (pikkukivi command verkko buk)
    (pikkukivi command verkko radio)
    (pikkukivi command verkko sget)
    ;; (pikkukivi command verkko pervo)
    (pikkukivi command verkko ylilauta)
    (pikkukivi command verkko sssh)))
