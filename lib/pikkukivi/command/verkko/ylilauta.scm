;; * ylilauta
(define-library (pikkukivi command verkko ylilauta)
    ;; ** exports
    (export ylilauta)
  ;; ** imports
  (import
    (scheme base)
    (scheme write)
    (gauche)
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
  ;; ** code
  (begin

    (define (usage)
      (print-strings
       '("Usage: ylilauta board thread"
         "  option:"
         "\t-a|all      get thread number from directories under cwd"
         "\t-r|repeat   repeat script with interval 5 minutes"
         "\tboard      only supports b,k,l,7,40"
         "\tthread      3839 2230 93988 482208 ..."
         "  expamle: "
         "\t$ ylilauta b 222222         # get images once from /b/222222 "
         "\t$ ylilauta -r id 9999       # get images from /id/9999 with repeat option"
         "\t$ ylilauta -a b             # get images from b with directory name as thread number"))
      (exit 2))

    (define (clear-list lst)
      (remove not lst))

    (define (parse-img-url line board)
      (let ((matched (rxmatch->string
                      (string->regexp
                       (string-append
                           "\/\/static.ylilauta.org\/files\/"
                         "\\w+\/orig\/[^\"]+"))
                      line)))
        (if (string? matched)
          (string-append "http:" matched)
          matched)))

    (define (get-image html board)
      (let ((image-url-list (clear-list
                             (call-with-input-string html
                                                     (lambda (in)
                                                       (port-map
                                                        (lambda (line)
                                                          (let ((m (parse-img-url line board)))
                                                            m))
                                                        (cut read-line in #true)))))))
        (flush)
        (let ((got-images (clear-list
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
                (sys-mkstemp "yli-temp")
                (http-get hostname path
                          :sink temp-out :flusher flusher)
                (close-output-port temp-out)
                (move-file temp-file file))
              #false)))))

    (define (get-thread-html board thread)
      (let-values (((status headers body)
                    (http-get "ylilauta.org"
                              (string-append "/"  board "/" thread))))
        (cond ((not (string=? status "404"))
               (let ((html (ces-convert body "*jp" "utf-8")))
                 (if (string-incomplete? html)
                   (string-incomplete->complete html :omit)
                   html)))
              (else  #false))))

    (define (ylilauta-get args)
      (let* ((board (car args))
             (thread (cadr args))
             (html (get-thread-html board thread)))
        (cond
          ((string? html)
           (display (paint thread 4))
           (mkdir thread)
           (cd thread)
           (get-image html board)
           (cd ".."))
          (else
              (print (paint (string-append thread "'s gone") 237))))))

    (define (ylilauta-get-all args)
      (let ((board (car args))
            (dirs (values-ref (directory-list2 (current-directory) :children? #true) 0)))
        (cond
          ((not (null? dirs))
           (for-each
               (lambda (d)
                 (ylilauta-get (list board d)))
             dirs)
           (run-process `(notify-send ,(string-append "ylilauta " board  " fetch finished"))))
          (else (print "no directories")))))

    (define (ylilauta-get-repeat args)
      (let* ((board (car args))
             (thread (cadr args))
             (html (get-thread-html board thread)))
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

    (define (ylilauta-get-repeat-all args)
      (loop-forever
       (let ((board (car args))
             (dirs (values-ref (directory-list2 (current-directory) :children? #true) 0)))
         (print (string-append "Board " (paint board 229)))
         (cond
           ((not (null? dirs))
            (for-each
                (lambda (d)
                  (ylilauta-get-repeat (list board d)))
              dirs))
           (else (print "no directories")))
         (tput-clr-bol)
         (print (paint "----------" 237)))))

    ;; ** main
    (define (ylilauta args)
      (let-args args
                ((all "a|all")
                 (repeat "r|repeat")
                 (else (opt . _) (print "Unknown option: " opt) (usage))
                 . restargs)
                (cond
                  ((null? restargs)
                   (usage))
                  ((and all repeat)
                   (ylilauta-get-repeat-all restargs))
                  (repeat
                   (loop-forever
                    (ylilauta-get-repeat restargs)))
                  (all
                   (ylilauta-get-all restargs))
                  (else
                      (ylilauta-get restargs)))))

    ))
