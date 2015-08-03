
(define-library (pikkukivi command verkko futaba main)
    (export futaba)

  (import(scheme base)
         (scheme write)
         (scheme file)
         (srfi 1)
         (srfi 11)
         (srfi 8)
         (srfi 37)
         (gauche base)
         (rfc http)
         (rfc uri)
         (gauche process)
         (gauche charconv)
         (file util)
         (util match)
         (gauche collection) ;find
         (gauche parseopt)
         (kirjasto komento työkalu)
         (kirjasto työkalu)
         (kirjasto merkkijono)
         (kirjasto pääte)
         (maali)
         )

  (begin

    (define (usage)
      (print-strings
       '("futaba board thread"
         ""
         "Usage:"
         "  futaba <board> <thread>       # get images once from /b/222222 "
         "  futaba -r <board> <thread>    # get images from /id/9999 with repeat option"
         "  futaba -a <board>             # get images from b with directory name as thread number"
         ""
         "Options:"
         "  -a --all      get thread number from directories under cwd"
         "  -r --repeat   repeat script with interval 5 minutes"
         ""
         "Arguments:"
         "  <board>      only supports b,k,l,7,40"
         "  <thread>      3839 2230 93988 482208 ..."))
      (exit 2))


    (define (parse-img-url line board)
      (rxmatch->string
       (string->regexp
        (string-append
            "http\:\/\/(\\w+)\\.2chan\\.net\/(\\w+)\/"
          board
          "\/src\/[^\"]+"))
       line))


    (define (get-image html board)
      (let ((image-url-list (remove not
                              (call-with-input-string html
                                                      (lambda (in)
                                                        (port-map
                                                         (lambda (line)
                                                           (parse-img-url line board))
                                                         (cut read-line in #true)))))))
        (set! html #false)
        (flush)
        (let ((got-images (remove not
                            (map (lambda (url) (fetch url))
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
                (sys-mkstemp "futaba-temp")
                (http-get hostname path
                          ':sink temp-out ':flusher flusher)
                (close-output-port temp-out)
                (move-file temp-file file))
              #false)))))

    (define (get-html board thread)
      (let-values (((status headers body)
                    (detect-server board thread)))
        (cond ((not (string=? status "404"))
               (let ((html (ces-convert body "*jp" "utf-8")))
                 (set! body #false)
                 (if (string-incomplete? html)
                   (string-incomplete->complete html ':omit)
                   html)))
              (else  #false))))

    (define (string->html-file thread body)
      (if body
        (begin
          (with-output-to-file
              (path-swap-extension thread "html")
            (lambda () (display body)))
          body)
        #false))

    (define (detect-server board thread)
      (let* ((fget (lambda (server)
                     (http-get (string-append server ".2chan.net")
                               (string-append "/" board "/res/" thread ".htm")))))
        (match board
               ("l" ;二次元壁紙
                (fget  "dat"))
               ("k" ;壁紙
                (fget "cgi"))
               ("b" ;虹裏
                (let ((servs '( "may" "jun" "dec"))
                      (get-res (lambda (srv)
                                 (receive (a b c)
                                   (fget srv)
                                   (if (not (string=? a "404")) srv #false))))
                      (get-values (lambda (srv)
                                    (receive (a b c)
                                      (fget srv)
                                      (when (not (string=? a "404")) (values a b c))))))
                  (or
                      (because ((s (get-res "may")))
                               (get-values s))
                    (because ((s (get-res "jun")))
                             (get-values s))
                    (because ((s (get-res "dec")))
                             (get-values s))
                    (values "404" #false #false))
                  ))
               ("7" ;ゆり
                (fget "zip"))
               ("16" ;二次元壁紙
                (fget "dat"))
               ("40" ;東方
                (fget "may"))
               ("p" ; お絵かき
                (fget "zip"))
               ("u"; 落書き裏
                (fget "cgi")))))

    (define (futaba-get args)
      (let* ((board (car args))
             (thread (cadr args))
             (html (get-html board thread)))
        (cond
          ((string? html)
           (display (paint thread 4))
           (mkdir thread)
           (cd thread)
           (string->html-file thread html)
           (get-image html board)
           (set! html #false)
           (cd ".."))
          (else
              (print (paint (string-append thread "'s gone") 237))))))

    (define (list-directories dir)
      (receive (dirs _x)
        (directory-list2 dir ':children? #true)
        (reverse dirs)))

    (define (futaba-get-all args)
      (let ((board (car args))
            (dirs (list-directories (current-directory))))
        (cond
          ((some? dirs)
           (for-each
               (lambda (d)
                 (futaba-get (list board d)))
             dirs)
           (run-process `(notify-send ,(string-append "futaba " board  " fetch finished"))))
          (else (print "no directories")))))

    (define (futaba-get-repeat args)
      (let* ((board (car args))
             (thread (cadr args))
             (html (get-html board thread)))
        (cond
          ((string? html)
           (tput-clr-bol)
           (display (paint thread 4))
           (mkdir thread)
           (cd thread)
           (string->html-file thread html)
           (get-image html board)
           (set! html #false)
           (cd ".."))
          (else
              (display (paint (string-append thread "'s gone") 237))
            (flush)
            (sys-select #false #false #false 100000)
            (display "\r")
            (tput-clr-eol)))))

    (define (futaba-get-repeat-all args)
      (forever
       (let ((board (car args))
             (dirs (list-directories (current-directory))))
         (print (string-append "Board " (paint board 229)))
         (cond
           ((some? dirs)
            (for-each
                (lambda (d)
                  (futaba-get-repeat (list board d)))
              dirs))
           (else (print "no directories")))
         (tput-clr-bol)
         (print (paint "----------" 237)))
       (sys-sleep 300)))

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

    (define (futaba args)
      (receive (help all repeat rest)
        (args-fold args
          options
          (lambda (option name arg . seeds)
            (println "Unknown option: " name)
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
           (futaba-get-repeat-all rest))
          (repeat
           (forever
            (futaba-get-repeat rest)))
          (all
           (futaba-get-all rest))
          (else
              (futaba-get rest)))

        (tput-cursor-normal)))

    ))

;; TODO support http://kazumi386.org:8801/b/mugon/bbsnote.cgi
