
;; -*- coding: utf-8 -*-

(define-module pikkukivi.commands.rm
  (export rm)
  (use file.util)
  (use gauche.parseopt))
(select-module pikkukivi.commands.rm)

(define (rm args)
  (let-args args
    ((verbose "v|verbose")
     . files)
    (cond
      (verbose
        (rm-verbose files))
      (else
        (rm-normal files)))))

(define (rm-normal files)
  (for-each
    (lambda (f)
      (cond ((file-is-symlink? f)
             (sys-remove f))
        (else
          (remove-directory* f))))
    files))


(define (rm-verbose files)
  (let loop ((files files))
    (cond
      ((null? files)
       '())
      (else
        (let ((file (car files)))
          (cond
            ((file-exists? file)
             (print file)
             (remove-files file)))
          (loop (cdr files)))))))
