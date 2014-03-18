;; (*- coding: utf-8 -*-

(define-library (pikkukivi commands launch-app)
    (export launch-app)
  (import
    (scheme base)
    (scheme write)
    (gauche base)
    (gauche process)
    (gauche parseopt)
    (util match)
    (file util))

  (begin

    (define additional-paths `(,(expand-path  "~/huone/ohjelmat/v2c")))

    (define (launch app)
      (if (or (find-file-in-paths (car app))
            (find-file-in-paths (car app)
                                :paths additional-paths))
        (begin
          (display "launching ")
          (display (car  app))
          (newline)
          (run-process app
                       :detached #true
                       :output :null
                       :error :null))
        (print (string-append "no such command "
                 (car app)))))

    (define (launch-app args)
      (launch args))))
