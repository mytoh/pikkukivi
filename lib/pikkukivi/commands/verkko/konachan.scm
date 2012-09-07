
(define-module pikkukivi.commands.verkko.konachan
  (export
    konachan
    )
  (use kirjasto)
  (use text.html-lite)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use gauche.parseopt)
  (use gauche.collection)
  (use srfi-1)
  (use rfc.http)
  (use clojure))
(select-module pikkukivi.commands.verkko.konachan)


(define (get-image-url html)
  (let ((parse (lambda (s) (rxmatch #/http\:\/\/konachan\.com\/(image|jpeg)\/[^"]+/ s))))
    (list (rxmatch-substring (parse html))
          (rxmatch-after (parse html)))))

(define (get-image-urls html)
  (let loop ((url (car (get-image-url html)))
             (match-after (cadr (get-image-url html))))
    (if (not url)
      '()
      (cons url (loop (car (get-image-url match-after))
                      (cadr (get-image-url match-after)))))))

(define (get-images page-number tag)
  (for-each
    swget
  (delete-duplicates
    (get-image-urls (last (find-all-tags "ul" (get-tags-page (+ page-number 1) tag)))))))

(define (parse-last-page-number s)
  (if-let1 pagination  (rxmatch->string #/<div class\=\"pagination\">.*?<\/div>/
                                        s)
    (let ((page (call-with-input-string  pagination  (lambda (in)
                                                       (ssax:xml->sxml in ())))))
      (caddr (find-max
               ((node-closure (ntype-names?? '(a))) page)
               :key (lambda (e) (x->number (caddr e))))))
    1))


(define (get-tags-page page-number tag)
  (receive (status head body)
    (http-get "konachan.com" (str "/post?page=" (number->string page-number) "&tags=" tag))
    body))

(define (get-tags-pages tag)
  (let ((last (x->number (parse-last-page-number (get-tags-page 1 tag)))))
    (dotimes (num last)
      (print (str (colour-string 99 "getting page ") (colour-string 33 (number->string (+ num 1)))))
      (get-images num tag))))

(define (konachan args)
  (let-args args
    ((tag "t|tag=s")
     . rest)
    (mkdir tag)
    (cd tag)
    (get-tags-pages tag)
    (cd ".."))
  )
