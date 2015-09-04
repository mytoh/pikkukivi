
(define-library (pikkukivi command ääliö main)
    (export
      ääliö)
  (import
    (scheme base)
    (scheme write)
    (scheme process-context)
    (gauche base)
    (gauche process) ; run-process
    (gauche parseopt)
    (srfi 13)
    (srfi 37)
    (util match)
    (file util) ; directory-list, current-directory
    (maali)
    (clojure)
    (kirjasto merkkijono)
    (kirjasto list)
    (kirjasto tiedosto)
    (rename (prefix (kirjasto tiedosto polku) path:)))

  (begin

    (define *ääliöpath*
      (let ((ääliöpath (get-environment-variable "ääliöpath")))
        (if ääliöpath
          ääliöpath
          (expand-path "~/huone/git/"))))

    (define (do-root)
      (println *ääliöpath*))

    ;; update git repository
    (define (do-update)
      (let ((repos (find-git-repository *ääliöpath*)))
        (for-each
            (lambda (r)
              (update-git-repository r))
          repos)
        (println "update finished!")))

    (define (update-git-repository dir)
      (message-update dir)
      (run-process `(git -C ,dir pull --ff --ff-only) ':wait #true)
      (newline))

    (define (message-update dir)
      (display (paint "=> " 4))
      (display (paint (path:child dir) 3))
      (display " ")
      (println (paint (string-drop dir (string-length *ääliöpath*))
                      39)))

    (define (git-repository? directory)
      (file-exists? (path:join directory ".git")))

    (define (find-git-repository directory)
      (flatten
       (map
           (lambda (d)
             (if (git-repository? d)
               d
               (find-git-repository d)))
         (directory-list2 directory
                          ':children? #true
                          ':add-path? #true))))

    ;; clean git repository with "git gc"
    (define (do-clean)
      (let ((dirs (list (directory-list (expand-path *ääliöpath*)
                                        ':children? #true
                                        ':add-path? #true))))
        (let loop ((dirs (car dirs)))
             (cond
               ((null? dirs)
                (display "cleaning finished!\n"))
               (else
                (cond
                  ((file-is-directory? (car dirs))
                   (display (paint "=> " 4))
                   (println (paint (path:child (car dirs)) 3))
                   (run-process '(git gc)
                                ':wait #true
                                ':directory (car dirs))
                   (newline))
                  (else  #true))
                (loop (cdr dirs)))))))



    (define (do-list args)
      (receive (full-path?)
        (args-fold (cdr args)
          options-list
          (lambda (option name arg . seeds)
            (error "Unrecognized option: " name))
          (lambda (operand full-path)
            (values full-path))
          #false)
        (let ((repos (find-git-repository *ääliöpath*)))
          (if full-path?
            (map
                println
              repos)
            (map
                (lambda (r)
                  (println (string-drop r (string-length *ääliöpath*))))
              repos)))))

    (define (do-usage status)
      (exit status "usage: ~a <command> <package-name>\n" "ääliö"))

    (define (do-get args)
      (receive (branch-name rest)
        (args-fold (cdr args)
          options-get
          (lambda (option name arg . seeds)
            (error "Unrecognized option: " name))
          (lambda (operand branch rest)
            (values branch (reverse (cons operand rest))))
          #false
          '())
        (let* ((path (format-url->path (car rest)))
               (git-url (format-url->git (car rest))))
          (println git-url)
          (if branch-name
            (run-process `(git clone --depth 1 -b ,branch-name ,git-url ,path)
                         ':wait #true)
            (run-process `(git clone --depth 1 ,git-url ,path)
                         ':wait #true)))))

    (define (url-is-github-short? url)
      (= 1 (string-count url #\/)))

    (define (url-is-github? url)
      (and (= 2 (string-count url #\/))
        (string-prefix? "github.com" url)))

    (define (format-url->path url)
      (cond
        ;; user/repo
        ((url-is-github-short? url)
         (string-append "github.com/" url))
        ;; github.com/user/repo
        ((url-is-github? url)
         url)
        (else
         (trim-url-prefix url))))

    (define (format-url->git url)
      (cond
        ((url-is-github-short? url)
         (string-append "git://github.com/" url))
        ((url-is-github? url)
         (string-append "git://" url))
        (else
         url)))

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
      (list (option '(#\h "help") (not 'required-arg?) (not 'optional-arg?)
                    (lambda (option name arg help)
                      (values #true)))))

    (define options-list
      (list (option '(#\p "full-path") (not 'required-arg?) (not 'optional-arg?)
                    (lambda (option name arg full-path)
                      (values #true)))))

    (define options-get
      (list (option '(#\b "branch") 'required-arg? (not 'optional-arg?)
                    (lambda (option name arg branch rest)
                      (values arg rest)))))

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
           (with-cwd *ääliöpath*
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
                             (do-get args))
                            ("root"
                             (do-root))
                            (_ (do-usage 1)))))))
      0)

    ))
