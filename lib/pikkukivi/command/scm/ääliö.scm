
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

    (define options-list
      (list (option '(#\p "full-path") (not 'require-arg?) (not 'optional-arg?)
                    (lambda (option name arg full-path)
                      (values #true)))))

    (define (do-list args)
      (receive (full-path?)
        (args-fold (cdr args)
          options-list
          (lambda (option name arg . seeds)
            (error "Unrecognized option:" name))
          (lambda (operand full-path)
            (values full-path))
          #false)
        (display full-path?)
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

    (define (usage status)
      (exit status "usage: ~a <command> <package-name>\n" *program-name*))

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
      (define (drop-prefix url prefix)
        (string-drop url (string-length prefix)))
      (cond ((string= "http://"
               (string-take url (string-length "http://")))
             (drop-prefix url "http://"))
            ((string= "https://"
               (string-take url (string-length "httsp://")))
             (drop-prefix url "https://"))
            ((string= "git://"
               (string-take url (string-length "git://")))
             (drop-prefix url "git://"))))


    (define (ääliö args)
      (let-args args
                ((#false "h|help" (usage 0))
                 . rest)
                (with-cwd *gitdir*
                          (match (car rest)
                                 ;; commands
                                 ((or "update" "up")
                                  (begin
                                    (print (string-append (paint "updating " 8) "repositories"))
                                    (do-update)))
                                 ("clean"
                                  (do-clean))
                                 ("list"
                                  (do-list rest))
                                 ("get"
                                  (do-get (cadr rest)))
                                 (_ (usage 1)))))
      0)

    ))
