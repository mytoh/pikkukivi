
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
      8chan
      shanachan
      sssh)
  (import
    (scheme base)
    (pikkukivi command verkko futaba main)
    (pikkukivi command verkko yotsuba main)
    (pikkukivi command verkko konachan)
    (pikkukivi command verkko lainchan main)
    (pikkukivi command verkko pahvi main)
    (pikkukivi command verkko buk)
    (pikkukivi command verkko radio main)
    (pikkukivi command verkko sget)
    ;; (pikkukivi command verkko pervo)
    (pikkukivi command verkko ylilauta main)
    (pikkukivi command verkko 8chan main)
    (pikkukivi command verkko shanachan main)
    (pikkukivi command verkko sssh)))
