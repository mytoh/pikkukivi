

(define-module pikkukivi.cli

  (export run)

  (use file.util)
  (use util.match)
  (use util.list)
  (use pikkukivi.commands)

  (begin

    (define (run args)
      (let ((command (cadr args))
            (rest (cddr args)))
        (match command
          ("futaba"
           (futaba rest))
          ("yotsuba"
           (yotsuba rest))
          ("pervo"
           (pervo rest))
          ("pahvi"
           (pahvi rest))
          ("commands"
           (commands rest)))))

    ))
