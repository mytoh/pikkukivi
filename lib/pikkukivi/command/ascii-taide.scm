
(define-library (pikkukivi command ascii-taide)
    (export
      ascii-taide)
  (import
    (scheme base)
    (gauche base)
    (gauche process)
    (util list) ; slices
    (util match)
    (file util)
    (srfi 1)                                        ; iota
    (srfi  13)
    (kirjasto tiedosto))

  (begin
    (define ascii-directory
      (build-path (home-directory) ".aa"))

    (define (file-is-aa? name)
      (if (string=? "aa" (path-extension name))
        #true
        #false))

    (define (ascii-taide args)
      (cat-file
       (map (lambda (f)
              (find-file-in-paths (path-swap-extension f "aa")
                                  :paths `(,(expand-path ascii-directory))
                                  :pred file-is-aa?))
         args)))))
