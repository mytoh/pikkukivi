

(define-module pikkukivi.alias
  (export
    alias-list)
  (use file.util)
  )
(select-module pikkukivi.alias)

(define alias-list
  (with-input-from-file
    (build-path (home-directory) ".pikkukivi/misc/alias.scm")
    read)  )
