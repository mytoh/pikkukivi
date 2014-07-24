
(define-library (pikkukivi command kuva)
    (export
      kuva)
  (import
    (scheme base)
    (scheme write)
    (gauche)
    (gauche process)
    (gauche parseopt)
    (util match)
    (file util)
    (kirjasto arkisto)
    (pikkukivi command unpack))
  (begin
    (define (usage status) (exit status "usage: ~a <file>\n" *program-name*))

    (define default-feh-options
      '(--auto-zoom --fullscreen --quiet --reverse --sort mtime))

    (define (open-directory dir)
      (run-process `(feh ,@default-feh-options ,dir) :wait #true))

    (define (open-regular-file file)
      (run-process `(feh ,@default-feh-options
                         --start-at
                         ,(sys-realpath file)
                         ,(sys-dirname (sys-realpath file)))
                   :wait #true))

    (define (open-archive file)
      (let ((temp (build-path
                   (temporary-directory)
                   "kuva"
                   (string-incomplete->complete
                    (sys-basename
                     (path-sans-extension file))))))
        (make-directory* temp)
        (unpack (list file temp))
        (run-process `(feh ,@default-feh-options  -r ,temp) :wait #true)
        (remove-directory* temp)))

    (define open
      (lambda (args)
        (cond ((null? args)
               (run-process `(feh) :wait #true))
              (else
                  (let-args args ((#false "h|help" (usage 0)) . rest)
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
