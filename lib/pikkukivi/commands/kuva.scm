

(define-module pikkukivi.commands.kuva
  (export
    kuva)
  (use gauche.process)
  (use gauche.parseopt)
  (use util.match)
  (use file.util)
  (use kirjasto.arkisto)
  (use pikkukivi.commands.unpack)
  )
(select-module pikkukivi.commands.kuva)

(define (usage status) (exit status "usage: ~a <file>\n" *program-name*))

(define feh
  (lambda (args)
    (cond ((null? args)
           (run-process `(feh) :wait #t))
      (else
        (let-args args ((#f "h|help" (usage 0)) . rest)
          (match (car rest)
            ((? file-is-directory? dir)
             (run-process `(feh "-F" ,dir)) :wait #t)
            ((? file-is-archive? file)
             (let ((temp (build-path
                           (temporary-directory)
                           "kuva"
                           (string-incomplete->complete
                             (sys-basename
                               (path-sans-extension file))))))
               (make-directory* temp)
               (unpack (list file temp))
               (run-process `(feh -F -Z -r -q ,temp) :wait #t)
               (remove-directory* temp)))
            ((? file-is-regular? file)
             (run-process `(feh -Z -F  -q --start-at
                                ,(sys-realpath file)
                                ,(sys-dirname (sys-realpath file)))
                          :wait #t))
            (_ (usage 1))))))))

(define (kuva args)
  (feh args))
