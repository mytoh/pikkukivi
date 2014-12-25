
(define-library (pikkukivi command swap-extension)
    (export
      swap-extension)
  (import
    (scheme base)
    (scheme file)
    (gauche base)
    (util match)
    (file util))
  (begin
    (define (swap-extension args)
      (match args
             ((srcext destext)
              (let ((src-files
                     (directory-list
                      (current-directory)
                      :children? #t
                      :filter (lambda (e) (and (not (file-is-directory? e))
                                            (equal? srcext (path-extension e)))))))
                (for-each
                    (lambda (f)
                      (print f)
                      (move-file f (path-swap-extension f destext)))
                  src-files)))))))
