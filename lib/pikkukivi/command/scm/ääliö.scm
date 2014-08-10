
 (define-library (pikkukivi command scm ääliö)
    (export
      ääliö)
  (import
    (scheme base)
    (scheme write)
    (gauche base)
    (gauche process) ; run-process
    (gauche parseopt)
    (srfi 13)
    (srfi 37)
    (util match)
    (file util) ; directory-list, current-directory
    (maali)
    (clojure)
    (kirjasto list)
    (kirjasto tiedosto))

  (begin

    (define *gitdir*  (expand-path "~/huone/git/"))

    (define (do-root)
      (display *gitdir*)
      (newline))

    ;; update git repository
    (define (do-update)
      (let ((repos (find-git-repository *gitdir*)))
        (for-each
            (lambda (r)
              (update-git-repository r))
          repos)
        (display "update finished!")
        (newline)))

    (define (update-git-repository dir)
      (display (paint "=> " 4))
      (display (paint (sys-basename dir) 3))
      (newline)
      (run-process `(git -C ,dir pull) :wait #true)
      (newline))

    (define (git-repository? directory)
      (file-exists? (build-path directory ".git")))

    (define (find-git-repository directory)
      (flatten
       (map
           (lambda (d)
             (if (git-repository? d)
               d
               (find-git-repository d)))
         (directory-list2 directory
                          :children? #true
                          :add-path? #true))))

    ;; clean git repository with "git gc"
    (define (do-clean)
      (let ((dirs (list (directory-list (expand-path *gitdir*) :children? #true :add-path? #true))))
        (let loop ((dirs (car dirs)))
             (cond
               ((null? dirs)
                (display "cleaning finished!\n"))
               (else
                   (cond
                     ((file-is-directory? (car dirs))
                      (display (paint "=> " 4))
                      (display (paint (sys-basename (car dirs)) 3))
                      (newline)
                      (run-process '(git gc) :wait #true :directory (car dirs))
                      (newline))
                     (else  #true))
                 (loop (cdr dirs)))))))



    (define (do-list args)
      (receive (full-path?)
        (args-fold (cdr args)
          options-list
          (lambda (option name arg . seeds)
            (error "Unrecognized option:" name))
          (lambda (operand full-path)
            (values full-path))
          #false)
        (let ((repos (find-git-repository *gitdir*)))
          (if full-path?
            (map
                (lambda (r)
                  (display r)
                  (newline))
              repos)
            (map
                (lambda (r)
                  (display
                      (string-drop r (string-length *gitdir*)))
                  (newline))
              repos)))))

    (define (do-usage status)
      (exit status "usage: ~a <command> <package-name>\n" "ääliö"))

    (define (do-get url)
      (let* ((path (format-url->path url))
             (full-path (string-append *gitdir* path))
             (git-url (format-url->git url)))
        (display git-url)
        (newline)
        (run-process `(git clone --depth 1 ,git-url ,path) :wait #true)))

    (define (format-url->path url)
      (cond
        ((= 1 (string-count url #\/))
         (string-append "github.com/" url))
        (else
            (trim-url-prefix url))))

    (define (format-url->git url)
      (cond
        ((= 1 (string-count url #\/))
         (string-append "git://github.com/" url))
        (else
            (string-append "git://" (trim-url-prefix url)))))

    (define (trim-url-prefix url)
      (define (drop-prefix u prefix)
        (string-drop u (string-length prefix)))
      (cond ((url-protocol=? "http" url)
             (drop-prefix url "http://"))
            ((url-protocol=? "https" url)
             (drop-prefix url "https://"))
            ((url-protocol=? "git" url)
             (drop-prefix url "git://"))))

    (define (url-protocol=? p url)
      (let ((proto (string-append p "://")))
        (string= proto
          (string-take url (string-length proto)))))

    (define options
      (list (option '(#\h "help") (not 'require-arg?) (not 'optional-arg?)
                    (lambda (option name arg help)
                      (values #true)))))

    (define options-list
      (list (option '(#\p "full-path") (not 'require-arg?) (not 'optional-arg?)
                    (lambda (option name arg full-path)
                      (values #true)))))

    (define (ääliö args)
      (receive (help)
        (args-fold args
          options
          (lambda (option name arg . seeds)
            #false)
          (lambda (operand help)
            (values help))
          #false ; default help
          )
        (cond
          (help
           (do-usage 0))
          (else
              (with-cwd *gitdir*
                        (match (car args)
                               ;; commands
                               ((or "update" "up")
                                (begin
                                  (print (string-append (paint "updating " 8) "repositories"))
                                  (do-update)))
                               ("clean"
                                (do-clean))
                               ("list"
                                (do-list args))
                               ("get"
                                (do-get (cadr args)))
                               ("root"
                                (do-root))
                               (_ (do-usage 1)))))))
      0)

    ))
