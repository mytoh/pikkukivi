
(define-library (pikkukivi commands verkko yotsuba)
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
          (file util)
          (util match)
          (srfi 1)
          (srfi  11)
          (srfi 13)
          (kirjasto komento työkalu)
          (kirjasto työkalu)
          (kirjasto merkkijono)
          (kirjasto pääte)
          (maali)
          (clojure))
  (begin

    (define (usage)
      (print-strings
       '("Usage: yotsuba board thread"
         "  option)"
         "\t-a|all      get thread number from directories under cwd"
         "\t-r|repeat   repeat script with interval 5 minutes"
         "\tboard       b g a v hc ..."
         "\tthread      3839 2230 93988 482208 ..."
         "  expamle) "
         "\t$ yotsuba b 999999        # get images from /b/999999 with repeat option"
         "\t$ yotsuba -r g 9999       # get images from /g/9999 with repeat option"
         "\t$ yotsuba -a b            # get images from b with directory name as thread number"))
      (exit 2))

    (define (parse-img line board)
      (or (rxmatch->string
           (string->regexp
            (str "\\/\\/[a-z]\\.4cdn\\.org\\/"
                 (x->string  board)
                 "\\/src\\/[^\"]+"))
           line)
        (rxmatch->string
         (string->regexp
          (str "\\/\\/images\\.4chan\\.org\\/"
               (x->string board)
               "\\/src\\/[^\"]+"))
         line)))

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
            (cond
              ((not (file-is-readable? file))
               (receive (temp-out temp-file)
                        (sys-mkstemp "yotsuba-temp")
                        (http-get hostname path
                                  :sink temp-out :flusher flusher)
                        (close-output-port temp-out)
                        (move-file temp-file file))
               file)
              (else #f))))))

    (define (get-img body board)
      (let ((img-url-list (delete-duplicates
                              (filter string? (map (lambda (x)
                                                     (parse-img x board))
                                                (string-split body
                                                              (string->regexp
                                                               "<\/?(?:img)[^>]*>")))))))
        (flush)
        (let ((got-images (remove not (map (lambda (url)
                                             ;; download indivisual image
                                             (fetch (str "http:" url)))
                                        img-url-list))))
          (match (length got-images)
                 (0 (newline))
                 (1 (print " " (paint (number->string (length got-images)) 49)
                           " file"))
                 (_ (print " " (paint (number->string (length got-images)) 49)
                           " files"))))))

    (define (get-html bd td)
      (let-values (((status headers body)
                    (http-get  "boards.4chan.org"
                               (str "/" (x->string bd) "/res/"  (x->string td)))))
        (cond
          ((string=? status "404")
           #f)
          ((string-incomplete? body)
           (if-let1 html (string-incomplete->complete body :omit)
                    html
                    (ces-convert body "*jp" "utf-8")))
          (else
              (ces-convert body "*jp" "utf-8")))))

    (define (string->html-file thread body)
      (with-output-to-file
          (path-swap-extension thread "html")
        (lambda () (display body)))
      body)

    (define (yotsuba-get args)
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
           (get-img html board)
           (cd ".."))
          (else
              (display (paint (str thread "'s gone") 103))
            (flush)
            (sys-select #f #f #f 100000)
            (display "\r")
            (tput-clr-eol)))))


    (define (yotsuba-get-all args)
      (let ((bd (car args))
            (dirs (values-ref (directory-list2 (current-directory) :children? #t) 0)))
        (cond
          ((not (null? dirs))
           (for-each
               (lambda (d)
                 (yotsuba-get (list bd d)))
             dirs)
           (print (paint bd 33) " fetch finished"))
          (else
              (print "no directories")))))

    (define (yotsuba-get-repeat args)
      (loop-forever
       (begin
         (yotsuba-get args))))

    (define (yotsuba-get-repeat-all args)
      (loop-forever
       (let ((bd (car args))
             (dirs (values-ref (directory-list2 (current-directory) :children? #t) 0)))
         (print "getting " bd)
         (if-not (null? dirs)
                 (for-each
                     (lambda (d)
                       (yotsuba-get (list bd d)))
                   dirs)
                 (print "no directories")))))

    (define (yotsuba args)
      (let-args args
                ((all "a|all")
                 (repeat "r|repeat")
                 (else (opt . _) (print "Unknown option: " opt) (usage))
                 . restargs)
                (tput-cursor-invisible)
                (cond
                  ((null? restargs)
                   (usage))
                  ((and all repeat)
                   (yotsuba-get-repeat-all restargs))
                  (repeat
                   (yotsuba-get-repeat restargs))
                  (all
                   (yotsuba-get-all restargs))
                  (else
                      (yotsuba-get restargs)))
                (tput-cursor-normal))))
  )
