
 (define-library (pikkukivi commands kuva)
    (export
      kuva)
  (import
    (scheme base)
    (scheme write)
    (gauche base)
    (gauche process)
    (gauche parseopt)
    (util match)
    (file util)
    (kirjasto arkisto)
    (pikkukivi commands unpack))
  (begin
    (define (usage status) (exit status "usage: ~a <file>\n" *program-name*))

    (define (open-directory dir)
      (run-process `(feh "-F" ,dir) :wait #t))

    (define (open-regular-file file)
      (run-process `(feh -Z -F  -q --start-at
                         ,(sys-realpath file)
                         ,(sys-dirname (sys-realpath file)))
                   :wait #t))

    (define (open-archive file)
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

    (define open
      (lambda (args)
        (cond ((null? args)
               (run-process `(feh) :wait #t))
              (else
                  (let-args args ((#f "h|help" (usage 0)) . rest)
                            (match (car rest)
                                   ((? file-is-directory? dir)
                                    (open-directory dir))
                                   ((? file-is-archive? file)
                                    (open-archive file))
                                   ((? file-is-regular? file)
                                    (open-regular-file file))
                                   (_ (usage 1))))))))

    (define (kuva args)
      (open args))))
