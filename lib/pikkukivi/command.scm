
(define-library (pikkukivi command)
    (export string->command)
  (import (scheme base)
          (pikkukivi command unpack)
          (pikkukivi command repl)
          (pikkukivi command rm)
          (pikkukivi command emma)
          (pikkukivi command colour)
          (pikkukivi command topless)
          (pikkukivi command ascii-taide)
          (pikkukivi command juosta)
          (pikkukivi command print-path)
          (pikkukivi command tmux-start)
          (pikkukivi command kuva)
          (pikkukivi command yes)
          (pikkukivi command swap-extension)
          (pikkukivi command piip)
          (pikkukivi command pomfup)
          (pikkukivi command vittu)

          (pikkukivi command verkko)
          (pikkukivi command scm)

          (pikkukivi command commands)
          (pikkukivi command help))

  (begin

    (define-syntax cond-command
      (syntax-rules ()
        ((_ command command* ...)
         (lambda (c)
           (cond
             ((equal? c (symbol->string 'command)) command)
             ((equal? c (symbol->string 'command*)) command*)
             ...)))))

    (define string->command
      (cond-command
       ascii-taide
       buk
       colour
       commands
       emma
       futaba
       help
       juosta
       konachan
       kuva
       pahvi
       pervo
       piip
       pomfup
       print-path
       radio
       repl
       rm
       sget
       sssh
       swap-extension
       tmux-start
       topless
       unpack
       vittu
       yes
       ylilauta
       yotsuba
       yotsuba-old
       ääliö
       ))
    ))
