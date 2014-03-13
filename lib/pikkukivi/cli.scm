
(define-library (pikkukivi cli)
    (export run)
  (import (scheme base)
          (file util)
          (util match)
          (util list)
          (pikkukivi commands))
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
               ("ylilauta"
                (ylilauta rest))
               ("commands"
                (commands rest))
               ("pomfup"
                (pomfup rest))
               ("colour"
                (colour rest))
               ("kuva"
                (kuva rest))
               ("print-path"
                (print-path rest))
               ("launch-app"
                (launch-app rest))
               ("konachan"
                (konachan rest)))))
    ))
