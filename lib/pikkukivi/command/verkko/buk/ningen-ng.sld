
 (define-library (pikkukivi command verkko buk ningen-ng)
    (export ningen-ng)
  (import
    (scheme base)
    (scheme file)
    (scheme write)
    (srfi 8)
    (gauche base)
    (rfc http)
    (gauche charconv)
    (gauche collection)
    (gauche parseopt)
    (file util))

  (begin

    (define (cd dir)
      (if (file-is-directory? dir)
        (current-directory dir)))

    (define (mkdir dir)
      (if (not (file-exists? dir))
        (make-directory* dir)))

    (define (get-thread thread)
      (receive (status head body)
        (http-get "xn--u8jm6cyd8028a.net"
                  (string-append "/1"
                    (http-compose-query "/imgboard.php" `((res ,thread)))))
        (ces-convert body "*jp" "utf-8")))

    (define (parse-image line)
      (let ((match
             (rxmatch->string #/src\/(\d+)[^\"]+/ line)))
        (if match
          (string-append "http://xn--u8jm6cyd8028a.net/1/" match)
          #false)))

    (define (parse-url url)
      (rxmatch-let (rxmatch #/^http:\/\/([-A-Za-z\d.]+)(:(\d+))?(\/.*)?/ url)
                   (#false host #false port path)
                   (values host port path)))

    (define (swget url)
      (receive (host port path) (parse-url url)
               (let ((file (receive (a fname ext) (decompose-path path)
                                    (string-append fname "." ext))))
                 (if (not (file-is-readable? file))
                   (http-get host path
                             :sink (open-output-file file)
                             :flusher (lambda (s h) (print file) #true))))))

    (define (get-image thread)
      (let ((html (get-thread thread)))
        (call-with-input-string html
                                (lambda (in)
                                  (port-for-each
                                   (lambda (line)
                                     (let ((match (parse-image line)))
                                       (if match
                                         (swget match))))
                                   (cut read-line in #true))))))

    (define (get-ningen-ng thread)
      (display "[0;34m")
      (print thread)
      (display "[0m")
      (mkdir thread)
      (cd thread)
      (get-image thread)
      (cd ".."))

    (define (get-ningen-ng-all)
      (let ((dirs (values-ref (directory-list2 (current-directory)
                                               :children? #true) 0)))
        (if (some? dirs)
          (for-each
              (lambda (d)
                (get-ningen-ng d))
            dirs)
          (print "no directries"))))


    (define (usage)
      (format (current-error-port)
        "Usage: ~a thread \n ex) ~a 8820\n -a|all : get images under current directries" *program-name* "script")
      (exit 2))

    (define (ningen-ng args)
      (let-args args
                ((all "a|all")
                 (else (opt . _) (print "Unknown optin: " opt) (usage))
                 . restargs)
                (cond
                  (all
                   (get-ningen-ng-all))
                  (else
                      (if (null? restargs)
                        (usage)
                        (get-ningen-ng (car restargs)))))))
    ))
