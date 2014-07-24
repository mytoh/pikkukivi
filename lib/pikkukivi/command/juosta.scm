
(define-library (pikkukivi command juosta)
    (export juosta)
  (import
    (scheme base)
    (scheme write)
    (gauche)
    (gauche process)
    (gauche parseopt)
    (util match)
    (file util))

  (begin

    (define additional-paths `(,(expand-path  "~/huone/ohjelmat/v2c")))

    (define (juosta-app command)
      (let ((app (car command)))
        (if (or (find-file-in-paths app)
              (find-file-in-paths app
                                  :paths additional-paths))
          (begin
            (display "launching ")
            (display app)
            (newline)
            (run-process command
                         :detached #true
                         :output :null
                         :error :null))
          (print (string-append "no such command "
                   app)))))

    (define (juosta args)
      (juosta-app args))))
