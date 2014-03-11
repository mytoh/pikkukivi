
 (define-library (pikkukivi commands verkko pahvi)
    (export pahvi)
  (import
    (scheme base)
    (scheme write)
    (scheme file)
    (gauche base)
    (gauche parseopt)
    (gauche collection)
    (sxml ssax)
    (sxml sxpath)
    (file util)
    (rfc http)
    (rfc uri)
    (srfi 1)
    (srfi 11)
    (kirjasto komento työkalu)
    (kirjasto merkkijono)
    (maali)
    (clojure))

  (begin

    (define *danbooru-host*
      "danbooru.donmai.us")

    (define (get-post-page page-number tag)
      (get-html-body *danbooru-host*
                     (str "/posts?page=" (number->string page-number) "&tags=" tag)))

    (define (get-html-body host parts)
      (receive (status head body)
               (http-get host parts)
               body))

    (define (swget uri id)
      (let-values (((scheme user-info hostname port-number path query fragment)
                    (uri-parse uri)))
        (let* ((orig-file (receive (a f ext) (decompose-path path) #`",|id|.,|ext|"))
               (flusher (lambda (sink headers)  #t)))
          (when (not (file-is-readable? orig-file))
            (receive (temp-out temp-file)
                     (sys-mkstemp "pahvi-temp")
                     (http-get hostname path
                               :sink temp-out
                               :flusher flusher)
                     (close-output-port temp-out)
                     (move-file temp-file orig-file))
            (print orig-file)))))

    (define (get-images id-num-list)
      (let loop ((lst  id-num-list))
           (when (not (null?  lst))
             (let ((id (car (car lst)))
                   (url (cadr (car lst))))
               (swget url id)
               (loop (cdr lst))))))

    (define (parse-last-page-number str)
      (if-let1 pagination (rxmatch->string #/<div class\=\"pagination\">.*?<\/div>/
                                           str)
               (let ((page (call-with-input-string  pagination (lambda (in)
                                                                 (ssax:xml->sxml in ())))))
                 (caddr (find-max
                         ((node-closure (ntype-names?? '(a))) page)
                         :key (lambda (e) (x->number (caddr e))))))
               1))

    (define (parse-next-page-number body)
      (let* ((paginator (rxmatch->string #/<div class\=\"paginator\">.*?<\/div>/
                                         body))
             (page
              (call-with-input-string paginator
                                      (lambda (in) (ssax:xml->sxml in ()))))
             (next-string (cadr (last ((node-closure (ntype-names?? '(a))) page)))))
        (if (equal? '(rel "next") (cadr next-string))
          (string->number (rxmatch->string #/\d+/ (car (cdaddr next-string))))
          #f)))

    (define (parse-line proc str)
      (let ((parse (lambda (in)
                     (port-map
                      (lambda (line)
                        (let ((match (proc line)))
                          (cond
                            (match (match 1))
                            (else #f))))
                      (cut read-line in #t)))))
        (remove not
          (delete-duplicates
              (call-with-input-string str
                                      parse)))))

    (define (parse-post-page-urls _post-page)
      (let ((parse-image-url
             (lambda (line)
               (#/><a href=\"(\/posts\/\d+[^\"]+)\"/
                      line))))
        (parse-line parse-image-url _post-page)))

    (define (parse-post-page-image-url body)
      (let* ((data (rxmatch->string #/Size: <a href=\"(\/data\/[^\"]+)\">/
                                    body 1)))
        (str "http://" *danbooru-host* data)))

    (define (parse-post-page-image-id body)
      (let ((id (rxmatch->string #/  <li>ID: (\d+)<\/li>/ body 1)))
        id))

    (define (parse-post-page-image-info body)
      (list
          (parse-post-page-image-id body)
        (parse-post-page-image-url body)))

    (define (get-posts-all _tag)
      (print (str  "Tag: " (paint _tag 33)))
      (let loop ((num 1))
           (print (paint (str "page: " (paint (number->string num) 34))))
           (let ((body (get-post-page num _tag)))
             (map
                 (lambda (url)
                   (let ((post (get-html-body *danbooru-host* url)))
                     (swget
                      (parse-post-page-image-url post)
                      (parse-post-page-image-id post))))
               (parse-post-page-urls body))
             (let ((next (parse-next-page-number body)))
               (if next
                 (loop (+ num 1)))))))

    (define (command-get-posts-all tag)
      (mkdir tag)
      (cd tag)
      (get-posts-all tag)
      (cd ".."))

    (define (usage status)
      (exit status "usage: ~a <command> <package-name>\n" "pahvi"))

    (define (pahvi args)
      (let-args args
                ((tag "t|tag=s")
                 (#f "h|help" (usage 0))
                 . rest)
                (command-get-posts-all tag)))
    ))
