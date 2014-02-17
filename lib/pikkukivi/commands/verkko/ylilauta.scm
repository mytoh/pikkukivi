(define-module pikkukivi.commands.verkko.ylilauta
  (export ylilauta)

  (use rfc.http)
  (use rfc.uri)
  (use gauche.process)
  (use gauche.charconv)
  (use file.util)
  (use util.match)
  (use gauche.collection) ;find
  (use gauche.parseopt)
  (use srfi-11)
  (use kirjasto.komento.työkalu)
  (use kirjasto.työkalu)
  (use kirjasto.merkkijono)
  (use kirjasto.pääte)
  (use maali)
  (require-extension (srfi 1))

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


    (define (parse-img-url line board)
      (rxmatch->string
       (string->regexp
        (string-append
            "http[s]?:\/\/static.ylilauta.org\/files\/"
          "\\w+\/orig\/\\d+.[^\"]+"          ))
       line))

    (define (get-image html board)
      (let ((image-url-list (remove not
                              (call-with-input-string html
                                                      (lambda (in)
                                                        (port-map
                                                         (lambda (line)
                                                           (let ((m (parse-img-url line board)))
                                                             m))
                                                         (cut read-line in #t)))))))
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
                 (flusher (lambda (sink headers)  #t)))
            (if (not (file-is-readable? file))
              (receive (temp-out temp-file)
                       (sys-mkstemp "yli-temp")
                       (http-get hostname path
                                 :sink temp-out :flusher flusher)
                       (close-output-port temp-out)
                       (move-file temp-file file))
              #f)))))

    (define (get-thread-html board thread)
      (let-values (((status headers body)
                    (http-get "ylilauta.org"
                              (string-append "/"  board "/" thread))))
        (cond ((not (string=? status "404"))
               (let ((html (ces-convert body "*jp" "utf-8")))
                 (if (string-incomplete? html)
                   (string-incomplete->complete html :omit)
                   html)))
              (else  #f))))

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
              (print (paint (string-append thread "'s gone") 237))))
        ))

    (define (ylilauta-get-all args)
      (let ((board (car args))
            (dirs (values-ref (directory-list2 (current-directory) :children? #t) 0)))
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
            (sys-select #f #f #f 100000)
            (display "\r")
            (tput-clr-eol)))))

    (define (ylilauta-get-repeat-all args)
      (loop-forever
       (let ((board (car args))
             (dirs (values-ref (directory-list2 (current-directory) :children? #t) 0)))
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
