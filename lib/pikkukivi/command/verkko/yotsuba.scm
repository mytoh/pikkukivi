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
          (srfi  11)
          (srfi 13)
          (srfi 43)
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
              ((not (file-is-readable? file))
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
                               (str "/" bd "/thread/"  (x->string td)))))
        (cond
          ((string=? status "404")
           #false)
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

    (define (yotsuba-get-all args)
      (let ((bd (car args))
            (dirs (values-ref (directory-list2 (current-directory) :children? #true) 0)))
        (cond
          ((not (null? dirs))
           (for-each
               (lambda (d)
                 (yotsuba-get-one (list bd d)))
             dirs)
           (print (paint bd 33) " fetch finished"))
          (else
              (print "no directories")))))

    (define (yotsuba-get-one-repeat args)
      (loop-forever
       (begin
         (yotsuba-get-one args))))

    (define (yotsuba-get-all-repeat args)
      (loop-forever
       (let ((bd (car args))
             (dirs (values-ref (directory-list2 (current-directory) :children? #true) 0)))
         (print "getting " bd)
         (if-not (null? dirs)
                 (for-each
                     (lambda (d)
                       (yotsuba-get-one (list bd d)))
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
                   (yotsuba-get-all-repeat restargs))
                  (repeat
                   (yotsuba-get-one-repeat restargs))
                  (all
                   (yotsuba-get-all restargs))
                  (else
                      (yotsuba-get-one restargs)))
                (tput-cursor-normal)))

    (define (api-thread board number)
      (let-values (((status headers body)
                    (http-get "api.4chan.org"
                              (string-append "/" board "/thread/" number ".json"))))
        (if (thread-exists status)
          (parse-json-string body)
          #false)))

    (define (post-tim post)
      (let ((tim (assoc "tim" post)))
        (if tim (cdr tim) #false)))

    (define (post-ext post)
      (let ((ext (assoc "ext" post)))
        (if ext (cdr ext) #false)))

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
                 (1 (print " " (paint (number->string (length got-images)) 49)
                           " file"))
                 (_ (print " " (paint (number->string (length got-images)) 49)
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
              (display (paint (str thread "'s gone") 103))
            (flush)
            (sys-select #false #false #false 100000)
            (display "\r")
            (tput-clr-eol)))))

    ))
