

(define-module pikkukivi.commands
  (extend
    pikkukivi.commands.unpack
    pikkukivi.commands.repl
    pikkukivi.commands.ls
    pikkukivi.commands.rm
    pikkukivi.commands.emma
    pikkukivi.commands.colour
    pikkukivi.commands.topless
    pikkukivi.commands.ascii-taide
    pikkukivi.commands.launch-app
    pikkukivi.commands.print-path
    pikkukivi.commands.tmux-start
    pikkukivi.commands.kuva
    pikkukivi.commands.yes
    pikkukivi.commands.swap-extension
    pikkukivi.commands.piip

    pikkukivi.commands.verkko
    pikkukivi.commands.scm

    pikkukivi.commands.commands
    pikkukivi.commands.help
    ))
(select-module pikkukivi.commands)
