
(define-library (pikkukivi command repl)
    (export
      repl
      info)
  (import
    (scheme base)
    (gauche base)
    (gauche interactive)
    (file util)
    (util list)
    (util match)
    (srfi 1)
    (gauche reload)
    (gauche interactive info)
    (kirjasto väri)
    (maali))
  (begin
    ;; colours
    (define *colours*
      '((number . 13)
        (list   . 11)
        (string . 63)
        (symbol . 163)
        (true   . 4)
        (false  . 1)
        ))


    ;; http://d.hatena.ne.jp/teppey/20120826/1345996327
    ;; thanks teppey
    (define-macro (info fn)
      (define (%info fn)
        (with-module gauche.interactive.info
                     (or (and-let* ([string? *pager*]
                                    [orig    *pager*]
                                    [alt (case (string->symbol (sys-basename *pager*))
                                           [(more) (^p (list orig "-p" #`"+/^ --.+: ,p"))]
                                           [(less) (^p (list orig "-p" #`"^ --.+: ,p"))]
                                           [(lv)   (^p (list orig #`"+/^ --.+: ,p"))]
                                           [else #false])])
                                   (dynamic-wind
                                       (^[] (set! *pager* (alt (regexp-quote (x->string fn)))))
                                     (^[] (info fn))
                                     (^[] (set! *pager* orig)))
                                   (values))
                       (info fn))))
      (let1 fn (if (pair? fn) (cadr fn) fn)
            `(,%info ',fn)))


    ;; reader
    (define reader
      #false)

    ;; evaluator
    (define evaluator
      #true)

    (define printer-colourize
      (lambda (result)
        (let ((make-colour (lambda (assoc-key s)
                             (paint
                              (x->string  s)
                              (assoc-ref *colours* assoc-key)))))
          (match (class-name (class-of result))
                 ;; list
                 ('<pair>
                  (make-colour 'list  result))
                 ('<list>
                  (make-colour 'list  result))

                 ;; symbol
                 ('<symbol>
                  (make-colour 'symbol  result))

                 ;; string
                 ('<string>
                  (make-colour 'string result))
                 ('<char>
                  (make-colour 'string result))

                 ;; number
                 ('<number>
                  (make-colour  'number  result))
                 ('<integer>
                  (make-colour  'number  result))

                 ;; boolean
                 ('<boolean>
                  (cond
                    (result
                     (make-colour 'true "#true"))
                    (else
                        (make-colour 'false "#false"))))
                 (_ result)))))

    (define printer
      (lambda results
        (map (lambda (r)
               (display (paint "=> " 39))

               (display
                   (printer-colourize r))

               (newline))
          results)))

    ;; prompter
    (define prompter
      (lambda ()
        (print (symbol->string (module-name (current-module))))
        (display (string-append (paint "# (" 238)
                   (paint "> " 67)))
        (flush)))


    (define (repl args)
      (read-eval-print-loop
       reader
       evaluator
       printer
       prompter
       ))

    ))
