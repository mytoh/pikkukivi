;;; yotsuba.scm

 (define-library (pikkukivi command verkko yotsuba)
    (export yotsuba)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (gauche base)
          (gauche process)
          (gauche collection) ;find
          (gauche parseopt)
          (gauche charconv)
          (rfc http)
          (rfc uri)
          (rfc json)
          (file util)
          (util match)
          (srfi 1)
          (srfi 8)
          (srfi 11)
          (srfi 13)
          (srfi 37)
          (srfi 43)
          (kirjasto komento työkalu)
          (kirjasto työkalu)
          (kirjasto merkkijono)
          (kirjasto pääte)
          (rename (prefix (kirjasto avain) avain:))
          (maali)
          )
  (begin

    (define (usage)
      (print-strings
       '("get yotsuba thread images"
         ""
         "Usage:"
         "  yotsuba <board> <thread>"
         "  yotsuba --repeat <board> <thread>"
         "  yotsuba --all <board> "
         "  yotsuba --repeat --all <board>"
         ""
         "Options:"
         "  -a --all     get thread number from directories under cwd"
         "  -r --repeat  repeat script with interval 5 minutes"))
      (exit 2))

    (define (url->filename url)
      (receive (a fname ext)
        (decompose-path (values-ref (uri-parse url) 4))
        (path-swap-extension fname ext)))

    (define (fetch uri)
      (if (string? uri)
        (let-values (((scheme user-info hostname port-number path query fragment)
                      (uri-parse uri)))
          (let* ((file (url->filename uri))
                 (flusher (lambda (sink headers)  #true)))
            (cond
              ((not (file-exists? file))
               (receive (temp-out temp-file)
                 (sys-mkstemp "yotsuba-temp")
                 (http-get hostname path
                           :sink temp-out :flusher flusher)
                 (close-output-port temp-out)
                 (move-file temp-file file))
               file)
              (else #false))))
        #false))

    (define (get-html bd td)
      (let-values (((status headers body)
                    (http-get  "boards.4chan.org"
                               (string-append "/" bd "/thread/" td))))
        (cond
          ((string=? status "404")
           #false)
          ((string-incomplete? body)
           (if-let1 html (string-incomplete->complete body :omit)
                    html
                    body))
          (else
              body))))

    (define (string->html-file thread body)
      (with-output-to-file
          (path-swap-extension thread "html")
        (lambda () (display body)))
      body)

    (define (yotsuba-get-all args)
      (let ((bd (car args))
            (dirs (values-ref (directory-list2 (current-directory) :children? #true) 0)))
        (cond
          ((not (null? dirs))
           (for-each
               (lambda (d)
                 (yotsuba-get-one (list bd d)))
             dirs)
           (println (paint bd 33) " fetch finished"))
          (else
              (println "no directories")))))

    (define (yotsuba-get-one-repeat args)
      (loop-forever
       (begin
         (yotsuba-get-one args))))

    (define (yotsuba-get-all-repeat args)
      (loop-forever
       (let ((bd (car args))
             (dirs (values-ref (directory-list2 (current-directory) :children? #true) 0)))
         (println "getting " bd)
         (if-not (null? dirs)
                 (for-each
                     (lambda (d)
                       (yotsuba-get-one (list bd d)))
                   dirs)
                 (println "no directories")))))

    (define (api-thread board number)
      (let-values (((status headers body)
                    (http-get "api.4chan.org"
                              (string-append "/" board "/thread/" number ".json"))))
        (if (thread-exists status)
          (parse-json-string body)
          #false)))

    (define (post-tim post)
      (avain:get "tim" post))

    (define (post-ext post)
      (avain:get "ext" post))

    (define (thread-exists status)
      (string=? status "200"))

    (define (make-post-image-url board post)
      (if (post-tim post)
        (string-append
            "http://images.4cdn.org/"
          board "/"
          (number->string (post-tim post))
          (post-ext post))
        '()))

    (define (get-img posts board)
      (let ((img-url-list (vector-map
                              (lambda (_index post)
                                (make-post-image-url board post))
                            posts)))
        (flush)
        (let ((got-images (remove not (map (lambda (url)
                                             ;; download indivisual image
                                             (fetch url))
                                        img-url-list))))
          (match (length got-images)
                 (0 (newline))
                 (1 (println " " (paint (number->string (length got-images)) 49)
                             " file"))
                 (_ (println " " (paint (number->string (length got-images)) 49)
                             " files"))))))

    (define (yotsuba-get-one args)
      (let* ((board (car args))
             (thread (cadr args))
             (response (api-thread board thread)))
        (cond
          (response
           (tput-clr-bol)
           (display (paint thread 4))
           (mkdir thread)
           (cd thread)
           (string->html-file thread (get-html board thread))
           (get-img (cdar response) board)
           (cd ".."))
          (else
              (display (paint (string-append thread "'s gone") 103))
            (flush)
            (sys-select #false #false #false 100000)
            (display "\r")
            (tput-clr-eol)))))

    (define options
      (list
          (option '(#\h "help") (not 'require-arg?) (not 'optional-arg?)
                  (lambda (option name arg help all repeat rest)
                    (values #true all repeat rest)))
        (option '(#\a "all") (not 'require-arg?) (not 'optional-arg?)
                (lambda (option name arg help all repeat rest)
                  (values help #true repeat rest)))
        (option '(#\r "repeat") (not 'require-arg?) (not 'optional-arg?)
                (lambda (option name arg help all repeat rest)
                  (values help all #true rest)))))

    (define (yotsuba args)
      (receive (help all repeat rest)
        (args-fold args
          options
          (lambda (option name arg . seeds)
            #false)
          (lambda (operand help all repeat rest)
            (values help all repeat (reverse (cons operand rest))))
          #false ; default help
          #false ; all
          #false ; repeat
          '()    ; rest
          )

        (tput-cursor-invisible)

        (cond
          ((null? rest)
           (usage))
          (help
           (usage))
          ((and all repeat)
           (yotsuba-get-all-repeat rest))
          (repeat
           (yotsuba-get-one-repeat rest))
          (all
           (yotsuba-get-all rest))
          (else
              (yotsuba-get-one rest)))

        (tput-cursor-normal)))

    ))
