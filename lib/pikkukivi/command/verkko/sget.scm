
(define-library (pikkukivi command verkko sget)
    (export sget)
  (import
    (scheme base)
    (scheme file)
    (gauche base)
    (gauche net)
    (file util)
    (rfc http))

  (begin
    (define (parse-url url)
      (rxmatch-let (rxmatch #/^http:\/\/([-A-Za-z\d.]+)(:(\d+))?(\/.*)?/ url)
                   (#false host #false port path)
                   (values host port path)))

    (define (get url)
      (receive (host port path) (parse-url url)
               (call-with-output-file
                   (receive (a fname ext) (decompose-path path) (string-append fname "." ext))
                 (lambda (out)
                   (http-get host path
                             :sink out :flusher (lambda _ #true))))))

    (define (sget args)
      (get (car args)))

    ))
