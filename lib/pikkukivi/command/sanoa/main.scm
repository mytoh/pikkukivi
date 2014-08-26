
 (define-library (pikkukivi command sanoa main)
    (export sanoa)
  (import
    (scheme base)
    (gauche process)
    (srfi 13)
    (kirjasto työkalu)
    (kirjasto merkkijono)
    (kirjasto komento))

  (begin

    (define (run lang word)
      (let1 url (string-append
                    "http://translate.google.com/translate_tts?ie=UTF-8&tl="
                  (string-upcase lang)
                  "&q="
                  word)
            (run-command `(mpv --really-quiet ,url))))

    (define (sanoa args)
      (run (car args)
           (cadr args)))

    )
  )
