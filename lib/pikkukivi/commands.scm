

(define-module pikkukivi.commands
  (extend
    pikkukivi.commands.talikko
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
    pikkukivi.commands.piste
    pikkukivi.commands.tmux-start
    pikkukivi.commands.commands
    pikkukivi.commands.kuva
    pikkukivi.commands.yes
    pikkukivi.commands.swap-extension
    pikkukivi.commands.piip

    pikkukivi.commands.verkko
    pikkukivi.commands.scm
    ))
(select-module pikkukivi.commands)
