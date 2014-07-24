;;; backup.scm

(define-library (pikkukivi command backup)
    (export backup)
  (import (scheme base)
          (scheme write)
          (scheme process-context)
          (kirjasto komento)
          (gauche)
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

    (define (file-tar-xz file)
      (string-append file ".tar.xz"))

    (define (archive outfile infile)
      (run-command `(tar --create --verbose --file ,outfile
                         -C ,(sys-dirname infile) ,(sys-basename infile))))

    (define (backup args)
      (archive (out-directory (file-tar-xz "vihko"))
               (home-directory ".org"))
      (archive (out-directory (file-tar-xz "minorhythm"))
               (home-directory "huone/radio/minorhythm"))
      (archive (out-directory (file-tar-xz "501st"))
               (home-directory "huone/radio/501st"))
      (archive (out-directory (file-tar-xz "v2c"))
               (home-directory ".v2c")))

    ))
