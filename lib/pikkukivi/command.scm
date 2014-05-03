
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
       futaba
       commands
       yotsuba
       pervo
       pahvi
       ylilauta
       pomfup
       colour
       kuva
       print-path
       launch-app
       konachan
       unpack
       repl
       piip
       vittu
       juosta
       ääliö))

    ))
