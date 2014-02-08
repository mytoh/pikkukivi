

(define-module pikkukivi.commands
  (export
    futaba
    commands
    yotsuba
    pervo
    pahvi
    )

  (use pikkukivi.commands.unpack)
  (use pikkukivi.commands.repl)
  (use pikkukivi.commands.rm)
  (use pikkukivi.commands.emma)
  (use pikkukivi.commands.colour)
  (use pikkukivi.commands.topless)
  (use pikkukivi.commands.ascii-taide)
  (use pikkukivi.commands.launch-app)
  (use pikkukivi.commands.print-path)
  (use pikkukivi.commands.tmux-start)
  (use pikkukivi.commands.kuva)
  (use pikkukivi.commands.yes)
  (use pikkukivi.commands.swap-extension)
  (use pikkukivi.commands.piip)

  (use pikkukivi.commands.verkko)
  (use pikkukivi.commands.scm)

  (use pikkukivi.commands.commands)
  (use pikkukivi.commands.help)
  )
(select-module pikkukivi.commands)
