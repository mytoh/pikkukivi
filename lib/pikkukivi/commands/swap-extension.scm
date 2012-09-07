

(define-module pikkukivi.commands.swap-extension
  (export
    swap-extension)
  (use util.match)
  (use file.util)
  )
(select-module pikkukivi.commands.swap-extension)


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
         src-files)))))
