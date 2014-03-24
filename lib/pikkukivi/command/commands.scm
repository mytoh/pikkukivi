
(define-library (pikkukivi command commands)
    (export
      commands)
  (import
    (scheme base)
    (gauche base)
    (gauche process)
    (util list) ; slices
    (util match)
    (file util)
    (srfi 1)
    (srfi  13))

  (begin
    (define (commands args)
      (let ((pk-commands
             (concatenate (map (lambda (m)
                                 (module-exports (find-module (car m))))
                            (append (library-fold 'pikkukivi.commands.* acons '())
                              (library-fold 'pikkukivi.commands.*.* acons '()))))))
        (for-each print
          (sort
           (delete-duplicates
               pk-commands)
           (lambda (x y) (string<?  (symbol->string x)
                           (symbol->string y)))))))))
