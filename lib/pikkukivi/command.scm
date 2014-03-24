
(define-library (pikkukivi command)
    (export
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
      ääliö)
  (import (scheme base)
          (pikkukivi command unpack)
          (pikkukivi command repl)
          (pikkukivi command rm)
          (pikkukivi command emma)
          (pikkukivi command colour)
          (pikkukivi command topless)
          (pikkukivi command ascii-taide)
          (pikkukivi command launch-app)
          (pikkukivi command print-path)
          (pikkukivi command tmux-start)
          (pikkukivi command kuva)
          (pikkukivi command yes)
          (pikkukivi command swap-extension)
          (pikkukivi command piip)
          (pikkukivi command pomfup)

          (pikkukivi command verkko)
          (pikkukivi command scm)

          (pikkukivi command commands)
          (pikkukivi command help)))
