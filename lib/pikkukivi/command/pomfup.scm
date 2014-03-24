
(define-library (pikkukivi command pomfup)
    (export pomfup)
  (import (scheme base)
          (scheme process-context)
          (scheme write)
          (gauche base)
          (gauche process)
          (srfi 13)
          (rfc http)
          (rfc json))
  (begin
    (define (post file)
      (receive (status header body)
        (http-post "pomf.se"
                   "/upload.php"
                   `(("files[]" :file ,file)))
        body))

    (define (parse-file-url-json json)
      (cdr (assoc "url"
             (vector-ref (cdr (assoc "files" (parse-json-string json)))
               0))))

    (define (compose-file-url name)
      (string-append "http://a.pomf.se/"
        name))

    (define (xclip str)
      (call-with-output-process
       "xclip -selection clipboard"
       (lambda (out) (display str out))))

    (define (upload files)
      (let ((urls (map
                      (lambda (file)
                        (compose-file-url
                         (parse-file-url-json
                          (post file))))
                    files)))
        (write urls)
        (string-join urls "\n")))

    (define (pomfup args)
      (let ((files args))
        (xclip (upload files))))))

;; Local Variables:
;; mode: scheme
;; End:
