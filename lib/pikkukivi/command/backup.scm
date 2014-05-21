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

    (define default-directory
      (make-parameter
          (home-directory "huone/varmuuskopio")))

    (define (out-directory file)
      (build-path (default-directory) file))

    (define (tar outfile infile)
      (run-command `(tar --create --verbose --file ,outfile ,infile)))

    (define (backup args)
      (tar (out-directory "vihko.tar.xz")
           (home-directory ".org"))
      (tar (out-directory "minorhythm.tar.xz")
           (home-directory "huone/radio/minorhythm")))

    ))
