
(define-library (pikkukivi command verkko shanachan main)
    (export shanachan)

  (import (scheme base)
          (scheme write)
          (scheme file)
          (gauche base)
          (rfc http)
          (rfc uri)
          (gauche process)
          (gauche charconv)
          (file util)
          (util match)
          (gauche parseopt)
          (kirjasto komento työkalu)
          (kirjasto työkalu)
          (kirjasto merkkijono)
          (kirjasto pääte)
          (maali)
          (srfi 1)
          (srfi 11)
          (srfi 8)
          (srfi 37)
          )

  (begin

    (define (usage)
      (print-strings
       '("shanachan board thread"
         ""
         "Usage:"
         "  shanachan <board> <thread>       # get images once from /b/222222 "
         "  shanachan -r <board> <thread>    # get images from /id/9999 with repeat option"
         "  shanachan -a <board>             # get images from b with directory name as thread number"
         ""
         "Options:"
         "  -a --all      get thread number from directories under cwd"
         "  -r --repeat   repeat script with interval 5 minutes"
         ""
         "Arguments:"
         "  <board>      only supports b,k,l,7,40"
         "  <thread>      3839 2230 93988 482208 ..."))
      (exit 2))

    (define (parse-image-url line board)
      (let loop ((res '())
                 (match (match-image-url line board)))
           (if match
             (loop (cons (match 1) res)
                   (match-image-url (match 'after) board))
             res)))

    ;; href="/happy/src/1407584368073.jpg">1

    (define (match-image-url line board)
      (rxmatch
       (string->regexp
        (string-append
            " href=\"(/" board
            "/src/[^\".]+.[^\"]+)\">"))
       line))

    (define (parse-image-url-list html board)
      (car (delete-duplicates
               (remove not
                 (call-with-input-string html
                                         (lambda (in)
                                           (port-map
                                            (lambda (line)
                                              (parse-image-url line board))
                                            (cut read-line in #true))))))))

    (define (get-image html board)
      (let ((image-url-list (parse-image-url-list html board)))
        (let ((got-images (remove not
                            (map (lambda (url)
                                   (fetch (string-append "https://shanachan.org" url)))
                              image-url-list))))
          (match (length got-images)
                 (0 (newline))
                 (1 (print (string-append " " (paint (number->string (length got-images)) 49)
                                          " file")))
                 (_ (print (string-append " " (paint (number->string (length got-images)) 49)
                                          " files")))))))

    (define (url->filename url)
      (receive (a fname ext)
        (decompose-path (values-ref (uri-parse url) 4))
        (path-swap-extension fname ext)))

    (define (fetch uri)
      (when (string? uri)
        (let-values (((scheme user-info hostname port-number path query fragment)
                      (uri-parse uri)))
          (let* ((file (url->filename uri))
                 (flusher (lambda (sink headers)  #true)))
            (if (not (file-is-readable? file))
              (receive (temp-out temp-file)
                (sys-mkstemp "shanachan-temp")
                (http-get hostname path
                          ':sink temp-out ':flusher flusher
                          ':secure #false)
                (close-output-port temp-out)
                (move-file temp-file file))
              #false)))))


    (define (get-html board thread)
      (let-values (((status headers body)
                    (http-get "shanachan.org"
                              (string-append
                                  "/" board "/res/" thread ".html")
                              ':secure #false)))
        (cond ((not (string=? status "404"))
               body)
              (else  #false))))

    (define (string->html-file thread body)
      (if body
        (begin
          (with-output-to-file
              (path-swap-extension thread "html")
            (lambda () (display body)))
          body)
        #false))

    (define (shanachan-get args)
      (let* ((board (car args))
             (thread (cadr args))
             (html (get-html board thread)))
        (cond
          ((string? html)
           (display (paint thread 4))
           (mkdir thread)
           (cd thread)
           (get-image html board)
           (string->html-file thread html)
           (cd ".."))
          (else
           (print (paint (string-append thread "'s gone") 237))))))

    (define (list-directories dir)
      (receive (dirs _x)
        (directory-list2 dir ':children? #true)
        dirs))

    (define (shanachan-get-all args)
      (let ((board (car args))
            (dirs (list-directories (current-directory))))
        (cond
          ((some? dirs)
           (for-each
               (lambda (d)
                 (shanachan-get (list board d)))
             dirs))
          (else (print "no directories")))))

    (define (shanachan-get-repeat args)
      (let* ((board (car args))
             (thread (cadr args))
             (html (get-html board thread)))
        (cond
          ((string? html)
           (tput-clr-bol)
           (display (paint thread 4))
           (mkdir thread)
           (cd thread)
           (get-image html board)
           (string->html-file thread html)
           (cd ".."))
          (else
           (display (paint (string-append thread "'s gone") 237))
           (flush)
           (sys-select #false #false #false 100000)
           (display "\r")
           (tput-clr-eol)))))

    (define (shanachan-get-repeat-all args)
      (forever
       (let ((board (car args))
             (dirs (list-directories (current-directory))))
         (print (string-append "Board " (paint board 229)))
         (cond
           ((some? dirs)
            (for-each
                (lambda (d)
                  (shanachan-get-repeat (list board d)))
              dirs))
           (else (print "no directories")))
         (tput-clr-bol)
         (print (paint "----------" 237)))))

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

    (define (shanachan args)
      (receive (help all repeat rest)
        (args-fold args
          options
          (lambda (option name arg . seeds)
            (display "Unknown option: " name)
            (newline)
            (usage))
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
           (shanachan-get-repeat-all rest))
          (repeat
           (forever
            (shanachan-get-repeat rest)))
          (all
           (shanachan-get-all rest))
          (else
           (shanachan-get rest)))

        (tput-cursor-normal)))

    ))
