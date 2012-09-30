
(define-module pikkukivi.commands.commands
  (export
    commands)
  (use gauche.process)
  (use util.list) ; slices
  (use util.match)
  (use file.util)
  (require-extension (srfi 1 13))    ; iota
  (use pikkukivi.alias)
  )
(select-module pikkukivi.commands.commands)

(define (commands args)
  (let ((pk-commands
          (concatenate (map (lambda (m)
                              (module-exports (find-module (car m))))
                            (append (library-fold 'pikkukivi.commands.* acons '())
                                    (library-fold 'pikkukivi.commands.*.* acons '())))))
        (alias-commands
          (map car alias-list)))
    (for-each print
      (sort
        (delete-duplicates
          (append pk-commands alias-commands))
        (lambda (x y) (string<?  (symbol->string x)
                                 (symbol->string y)))))))
