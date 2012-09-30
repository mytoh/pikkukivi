
(define-module pikkukivi.commands.yes
  (export
    yes)
  (use util.match)
  )
(select-module pikkukivi.commands.yes)


(define (yes args)
  (match args
    ((text)
     (let loop ()
       (print text)
       (loop)))
    (()
     (let loop ()
       (print "y")
       (loop))))
  )
