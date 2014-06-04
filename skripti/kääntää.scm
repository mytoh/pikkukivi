":"; exec gosh -- $0 "$@"

(use gauche.process)
(use gauche.parseopt)
(use gauche.charconv)
(use rfc.http)
(use rfc.json)
(use file.util)
(use kirjasto)
(use maali)
(require-extension (srfi 13))


(define get-google-translate
  (lambda (sl tl text)
    (rxmatch->string #/\"([^\"]*)\"/
                     (string-delete
                       (ces-convert
                         (values-ref (http-get "translate.google.com"
                                               (http-compose-query
                                                 "/translate_a/t"
                                                 `((client "t")
                                                   (ie "UTF-8")
                                                   (sl ,sl)
                                                   (tl ,tl)
                                                   (text ,text))))
                           2)
                         "iso-8859-1")
                       #[\[\],])
                     1)))

(define (save-to-dict slang tlang source trans)
  (cond
    ((not (file-exists? (build-path (home-directory)
                                    ".sdic" slang tlang)))
     (make-directory* (build-path (home-directory)
                                  ".sdic" slang))
     (call-with-output-file
       (build-path (home-directory)
                   ".sdic" slang tlang)
       (lambda (out)
         (format out "~s\n" (list source trans)))
       :if-exists :append))
    (else
      (call-with-output-file
        (build-path (home-directory)
                    ".sdic" slang tlang)
        (lambda (out)
          (format out "~s\n" (list source trans)))
        :if-exists :append))))

(define (main args)
  (let* ((source-lang (cadr args))
         (target-lang   (caddr args))
         (text      (cadddr args))
         (translated (get-google-translate source-lang target-lang text))
         )
    (save-to-dict source-lang target-lang text translated)
    (print
      (string-append
        text
        " -> "
        (paint translated 123 )))))



; http://translate.google.com/translate_a/t?client=t&ie=UTF-8&text=talikko&sl=fi&tl=en
