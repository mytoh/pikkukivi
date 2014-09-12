
 (define-library (pikkukivi command kuva main)
    (export
      kuva)
  (import
    (scheme base)
    (scheme write)
    (srfi 8)
    (srfi 37)
    (gauche base)
    (gauche process)
    (gauche parseopt)
    (file util)
    (kirjasto arkisto)
    (kirjasto työkalu)
    (pikkukivi command unpack))
  (begin
    (define (usage status) (exit status "usage: ~a <file>\n" "kuva"))

    (define feh-default-options
      '(--auto-zoom --fullscreen --quiet --reverse --sort mtime))

    (define sxiv-directory-options
      '(-s f -f -b -r))

    (define (open-directory-feh dir)
      (run-process `(feh ,@feh-default-options ,dir) ':wait #true))

    (define (open-directory-sxiv dir)
      (run-process `(sxiv ,@sxiv-directory-options ,dir)
                   ':wait #true))

    (define (open-directory dir)
      (open-directory-sxiv dir))

    (define (open-regular-file-feh file)
      (run-process `(feh ,@feh-default-options
                         --start-at
                         ,(sys-realpath file)
                         ,(sys-dirname (sys-realpath file)))
                   ':wait #true))

    (define (open-regular-file-sxiv file)
      (run-process `(sxiv ,@sxiv-directory-options
                          ,(sys-dirname file))
                   ':wait #true))

    (define (open-regular-file file)
      (open-regular-file-sxiv file))

    (define (open-archive file)
      (let ((temp (build-path
                   (temporary-directory)
                   "kuva"
                   (string-incomplete->complete
                    (sys-basename
                     (path-sans-extension file))))))
        (make-directory* temp)
        (unpack (list file temp))
        (run-process `(feh ,@feh-default-options  -r ,temp) ':wait #true)
        (remove-directory* temp)))

    (define options
      (list (option '(#\h "help") (not 'require-arg?) (not 'optional-arg?)
                    (lambda (option name arg help)
                      (values #true)))))

    (define (open args)
      (cond ((null? args)
             (open-directory "."))
            (else
                (receive (help)
                  (args-fold args
                    options
                    (lambda (option name arg . seeds)
                      (error "Unrecognized option:" name))
                    (lambda (operand help)
                      (values help))
                    #false ; default help
                    )
                  (cond
                    (help
                     (usage 0))
                    (else
                        (let ((file (car args)))
                          (fcond file
                                 (file-is-directory?
                                  (open-directory file))
                                 (file-is-archive?
                                  (open-archive file))
                                 (file-is-regular?
                                  (open-regular-file file))
                                 (else (usage 1))))))))))

    (define (kuva args)
      (open args))

    ))
