
(define-library (pikkukivi command verkko futaba)
    (export futaba)

  (import(scheme base)
         (scheme write)
         (scheme file)
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
         (srfi 1)
         (srfi 11))

  (begin

    (define (usage)
      (print-strings
       '("Usage: futaba board thread"
         "  option:"
         "\t-a|all      get thread number from directories under cwd"
         "\t-r|repeat   repeat script with interval 5 minutes"
         "\tboard      only supports b,k,l,7,40"
         "\tthread      3839 2230 93988 482208 ..."
         "  expamle: "
         "\t$ futaba b 222222         # get images once from /b/222222 "
         "\t$ futaba -r id 9999       # get images from /id/9999 with repeat option"
         "\t$ futaba -a b             # get images from b with directory name as thread number"))
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
                                                           (let ((m (parse-img-url line board)))
                                                             m))
                                                         (cut read-line in #true)))))))
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
                          :sink temp-out :flusher flusher)
                (close-output-port temp-out)
                (move-file temp-file file))
              #false)))))

    (define (get-html board thread)
      (let-values (((status headers body)
                    (detect-server board thread)))
        (cond ((not (string=? status "404"))
               (let ((html (ces-convert body "*jp" "utf-8")))
                 (if (string-incomplete? html)
                   (string-incomplete->complete html :omit)
                   html)))
              (else  #false))))

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
                (let ((servs '("jun" "dec" "may"))
                      (get-res (lambda (srv)
                                 (receive (a b c)
                                   (fget srv)
                                   (if (not (string=? a "404")) srv #false))))
                      (get-values (lambda (srv)
                                    (receive (a b c)
                                      (fget srv)
                                      (when (not (string=? a "404")) (values a b c))))))
                  (or (and-let* ((s (get-res "jun")))
                                (get-values "jun"))
                    (and-let* ((s (get-res "dec")))
                              (get-values "dec"))
                    (and-let* ((s (get-res "may")))
                              (get-values "may"))
                    (values "404" #false #false))))
               ("7" ;ゆり
                (fget "zip"))
               ("16" ;二次元壁紙
                (fget "dat"))
               ("40" ;東方
                (fget "may")))))

    (define (futaba-get args)
      (let* ((board (car args))
             (thread (cadr args))
             (html (get-html board thread)))
        (cond
          ((string? html)
           (display (paint thread 4))
           (mkdir thread)
           (cd thread)
           (get-image html board)
           (cd ".."))
          (else
              (print (paint (string-append thread "'s gone") 237))))))

    (define (list-directories dir)
      (receive (dirs _x)
        (directory-list2 dir :children? #true)
        dirs))

    (define (futaba-get-all args)
      (let ((board (car args))
            (dirs (list-directories (current-directory))))
        (cond
          ((not (null? dirs))
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
           (get-image html board)
           (cd ".."))
          (else
              (display (paint (string-append thread "'s gone") 237))
            (flush)
            (sys-select #false #false #false 100000)
            (display "\r")
            (tput-clr-eol)))))

    (define (futaba-get-repeat-all args)
      (loop-forever
       (let ((board (car args))
             (dirs (list-directories (current-directory))))
         (print (string-append "Board " (paint board 229)))
         (cond
           ((not (null? dirs))
            (for-each
                (lambda (d)
                  (futaba-get-repeat (list board d)))
              dirs))
           (else (print "no directories")))
         (tput-clr-bol)
         (print (paint "----------" 237)))))

    (define (futaba args)
      (let-args args
                ((all "a|all")
                 (repeat "r|repeat")
                 (else (opt . _) (print "Unknown option: " opt) (usage))
                 . restargs)
                (cond
                  ((null? restargs)
                   (usage))
                  ((and all repeat)
                   (futaba-get-repeat-all restargs))
                  (repeat
                   (loop-forever
                    (futaba-get-repeat restargs)))
                  (all
                   (futaba-get-all restargs))
                  (else
                      (futaba-get restargs)))))

    ))