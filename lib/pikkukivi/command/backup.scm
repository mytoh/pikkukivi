;;; backup.scm

(define-library (pikkukivi command backup)
    (export backup)
  (import (scheme base)
          (scheme write)
          (scheme process-context)
          (kirjasto komento)
          (file util))

  (begin

    (define (home)
      (get-environment-variable "HOME"))

    (define (home-directory file)
      (build-path (home) file))

    (define *default-directory*
      (make-parameter
          (home-directory "huone/varmuuskopio")))

    (define (out-directory file)
      (build-path (*default-directory*) file))

    (define (tar outfile infile)
      (run-command `(tar --create --verbose --file ,outfile ,infile)))

    (define (file-tar-xz file)
      (string-append file ".tar.xz"))

    (define (backup args)
      (tar (out-directory (file-tar-xz "vihko"))
           (home-directory ".org"))
      (tar (out-directory (file-tar-xz "minorhythm"))
           (home-directory "huone/radio/minorhythm"))
      (tar (out-directory (file-tar-xz "501st"))
           (home-directory "huone/radio/501st")))

    ))
