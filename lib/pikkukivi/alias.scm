
(define-library (pikkukivi alias)
    (export
      alias-list)
  (import
    (scheme base)
    (gauche)
    (file util))

  (begin
    (define alias-list
      (with-input-from-file
          (build-path (home-directory) ".pikkukivi/misc/alias.scm")
        read))))
