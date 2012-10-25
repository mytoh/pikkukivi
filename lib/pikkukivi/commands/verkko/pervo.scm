
(define-module pikkukivi.commands.verkko.pervo
  (export pervo)
  (use rfc.http)
  (use rfc.uri)
  (use rfc.cookie)
  (use rfc.md5)
  (use util.digest)
  (use util.list)
  (use text.tree)
  (use kirjasto.verkko)
  (use sxml.ssax)
  (use srfi-1)
  (use srfi-27)
  (use srfi-42)
  (use srfi-13)
  (use math.mt-random)
  (use file.util)
  (use kirjasto.merkkijono)
  (use gauche.sequence)
  )
(select-module pikkukivi.commands.verkko.pervo)

(define (alist->ns-cookie-string alist)
  (tree->string
    (intersperse ";"
                 (map (lambda (x) (intersperse "=" x)) alist))))

(define *interval* 2)

(define member-id
  (let ((m (make <mersenne-twister> :seed (sys-time))))
    (rxmatch->string #/(\d){6}/ (number->string (+ (mt-random-real m) (mt-random-real m))))))

(define *eh-cookie*
  (alist->ns-cookie-string
    `(("ipb_pass_hash" ,(digest-hexify (md5-digest-string member-id)))
      ("ipb_member_id" ,member-id)
      ("path" "/" )
      ("domain" ".exhentai.org" )
      ("nw" 1)
      ("tips" 1))))

(define *download-directory*
  "download")

(define parse-image-url
  (lambda (line)
    (sys-sleep *interval*)
    (rxmatch->string #/\"http\:\/\/(\d+(\.)?)+(:\d+)?\/[^\"]+(jpg|gif|png)\"/ line)))

(define (parse-single-pages html)
  (map
    (lambda (s)
      (rxmatch->string #/http\:\/\/\w+\.org\/s\/(\w+)\/(\d+)-(\d+)/ s))
    (filter
      (lambda (s)
        (string-scan s "gdtm"))
      (find-all-tags "div" html))))

(define (get-page url number)
  (surl (build-path url (string-append "?p=" (number->string number)))))

(define (parse-last-page-number html)
  (/ (length (filter (lambda (s) (#/http\:\/\/\w+\.org\/\w\/(\d+)\/(\w+)\/\?p\=\d/ s))
                     (find-all-tags "a" html)))
     2))

(define (surl uri)
  (let-values (((scheme user-info hostname port-number path query fragment)
                (uri-parse uri)))
    (values-ref (http-get hostname (or (uri-compose :path path :query query) "/")
                          :cookie *eh-cookie*)
      2)))

(define (get-image index directory url)
  (sys-sleep *interval*)
  (let* ((ext (path-extension (string-trim-both url #\")))
         (file (build-path (path-swap-extension (number->string index) ext)))
         (dir  (build-path *download-directory* directory))
         (download-path (build-path dir file)))
    (print download-path)
    (when (not (file-exists? dir))
      (make-directory* dir))
    (if (file-exists? download-path)
      (print download-path " exists!")
      (save-image (string-trim-both url #\") download-path))))

(define (save-image url download-path)
  (print url)
  (call-with-output-file download-path
    (lambda (in)
      (let-values (((scheme user-info hostname port-number path query fragment)
                    (uri-parse (string-trim-both url #\"))))
        (values-ref
          (http-get (if port-number
                      (string-append hostname ":" (number->string port-number))
                      hostname)
                    path
                    :cookie *eh-cookie*
                    :sink in
                    :flusher (lambda _ #t))
          2)))))

(define (make-save-directory-name html)
  (regexp-replace-all
    #/\<\/?\w+(\s+id\=\"\w+\")?\>/
    (car (find-all-tags "h1" html))
    ""))

(define (pervo args)
  (let* ((index-page (surl (car args)))
         (last-page-number (parse-last-page-number index-page))
         (single-pages
           (append-ec (:range i last-page-number)
             (parse-single-pages (get-page (car args) i))))
         (directory-name (make-save-directory-name index-page)))
    (for-each-with-index
      (lambda (page-number url)
        (guard (exc
                 ((<system-error> exc)
                  (get-image
                    page-number directory-name
                    (parse-image-url (surl url)))))
        (get-image
          page-number directory-name
          (parse-image-url (surl url)))))
      single-pages)))
