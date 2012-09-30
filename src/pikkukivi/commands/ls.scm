#!/usr/bin/env gosh

;; lesser copy of ls++ by trapd00r
;; colour codes are hardcoded so edit this file

(define-module pikkukivi.commands.ls
  (export
    ls)
  (use gauche.parseopt)
  (use gauche.process)
  (use gauche.sequence) ;remove
  (use gauche.charconv)
  (use util.match)
  (use util.list) ;take*
  (use file.util)
  (require-extension (srfi 1 13)) ;count
  (use clojure))
(select-module pikkukivi.commands.ls)


(load (build-path (home-directory) ".ls/config"))
(define
  *colours*
  (with-input-from-file
    (build-path (home-directory) ".ls/themes" *colour-theme*)
    read))



(define (convert-jp-filename name)
  (ces-convert name (ces-guess-from-string name "*jp")))

(define (list-files directory)
  (map convert-jp-filename
       (directory-list directory :children? #t :add-path? #t)))

(define (ls-make-colour colour stg)
  (cond
    ((<= colour (vector-length *colours*))
     (let1 c  (ref *colours* colour #f)
       ((lambda (colour s)
          (str "[38;5;" colour "m"  s "[0m"))
        c stg)))
    (else
      ((lambda (colour s)
         (str "[38;5;" colour "m"  s "[0m"))
       colour stg))))

(define (colour-filename name type . ecolour)
  (cond
    ((not (null? ecolour))
     (let1 e   (car  ecolour)
       (match type
         ('regular
          (if (file-is-executable? name)
                      (str  (ls-make-colour e name) (ls-make-colour 2 "*"))
                      (ls-make-colour e name)))
         ('directory
          (str (ls-make-colour e name) (ls-make-colour 12 "/")))
         ('symlink
          (str (ls-make-colour e name) (ls-make-colour 2 "@")))
         (_        (ls-make-colour e name)))))
    (else
      (match type
        ('regular
         (if (file-is-executable? name)
                       #`",(ls-make-colour 4 name),(ls-make-colour 2 \"*\")"
                       (colour-normal-file name)))
        ('directory
         #`",(ls-make-colour 1 name),(ls-make-colour 12 \"/\")")
        ('character
         (ls-make-colour 2 name))
        ('block
         (ls-make-colour 3 name))
        ('fifo
         (ls-make-colour 4 name))
        ('symlink
         #`",(ls-make-colour 5 name),(ls-make-colour 2 \"@\")")
        ('socket
         (ls-make-colour 6 name))
        (_        (ls-make-colour 2 name))))))

(define (colour-normal-file name)
  (let ((colour (filter
                  (lambda (r) ((string->regexp (car r)) name))
                  *normal-file-colours*)))
    (cond
      ((null? colour)
      (ls-make-colour 14 name))
      (else
    (ls-make-colour (cadar colour) name)))))


;; componets for display {{{

(define (print-filename filename stat)
  (let*  ((file (sys-basename filename))
          (type (~ stat 'type))
          (realname (sys-realpath filename))
          (extension  (path-extension file)))
    (case type
      ((symlink)  (cond
                    (extension
                      (if-let1 ext (ref *extension-colours* (string->symbol extension) #f)
                        (str (colour-filename file type ext) " -> " (ls-make-colour 10 realname))
                        (str (colour-filename file type) " -> " (ls-make-colour 10 realname))))
                    (else
                      (str (colour-filename file type) " -> " (ls-make-colour 10 realname)))))
      (else (cond
              (extension
                (if-let1 ext (ref  *extension-colours* (string->symbol extension) #f)
                  (colour-filename file type ext)
                  (colour-filename file type)))
              (else
                (colour-filename file type)))))))


(define (print-permission f stat)
  (let* ((perm (format #f "~3O" (ref stat 'perm)))
         (type (ref stat 'type))
         (lst (map (lambda (e) (if (char-numeric? e) e #\0)) (string->list perm)))
         (p (string-join  (quasiquote
                            ,(map (lambda (char)
                                    (let ((c (digit->integer char))
                                          (n (ls-make-colour 0 "-"))
                                          (r (ls-make-colour 6 "r"))
                                          (w (ls-make-colour 7 "w"))
                                          (x (ls-make-colour 5 "x")))
                                      (match c
                                        (0 #`",n,n,n")
                                        (1 #`",n,n,x")
                                        (2 #`",n,w,n")
                                        (3 #`",n,w,x")
                                        (4 #`",r,n,n")
                                        (5 #`",r,n,x")
                                        (6 #`",r,w,n")
                                        (7 #`",r,w,x"))))
                                  lst))
                          "")))
    (match type
      ('directory (str (ls-make-colour 1 "d") p))
      ('block     (str (ls-make-colour 2 "b") p))
      ('character (str (ls-make-colour 3 "c") p))
      ('symlink   (str (ls-make-colour 4 "l") p))
      ('fifo      (str (ls-make-colour 6 "p") p))
      ('socket    (str (ls-make-colour 7 "s") p))
      (_          (str (ls-make-colour 0 "-") p)))))

(define (print-size file stat)
  (let* ((filesize (ref stat 'size))
         (size (cond
                 ((> filesize 1073741824)
                  (str (ls-make-colour 7 (truncate (/ (/ (/ filesize 1024) 1024) 1024))) (ls-make-colour 3 "G")))
                 ((> filesize 1048576)
                  (str (ls-make-colour 7  (truncate (/ (/ filesize 1024) 1024))) (ls-make-colour 7 "M")))
                 ((> filesize 1024)
                  (str (ls-make-colour 7  (truncate (/ filesize 1024))) (ls-make-colour 2 "K")))
                 ((< filesize 1024)
                  (str (ls-make-colour 7  filesize) (ls-make-colour 14 "B")))
                 (else
                  (str (ls-make-colour 7  filesize) (ls-make-colour 14 "B")))
                 )))
    (format "~35@a"
            size)))


(define-constant *delimiters*
    (vector
    (ls-make-colour 0 "├")
    (ls-make-colour 0 "┤")
    (ls-make-colour 0 "│"))
  )
(define (print-delimiter num)
  (vector-ref *delimiters* (- num 1)))

(define (print-owner file stat)
  (let* ((user (if-let1 u (sys-uid->user-name (ref stat 'uid)) u (ref stat 'uid)))
        (group (sys-gid->group-name (ref stat 'gid))))
  (format "~a:~a"
          (ls-make-colour 2 user)
          (ls-make-colour 6 group))))

(define (print-time-format unit colour time)
  (match unit
    ('sec (format "~17@a ~a" (ls-make-colour colour time) (ls-make-colour colour "sec ")))
    ('min (format "~17@a ~a" (ls-make-colour colour (round->exact (/. time 60))) (ls-make-colour colour "min ")))
    ('hour (format "~17@a ~a" (ls-make-colour colour (round->exact (/. (round->exact (/. time 60)) 60))) (ls-make-colour colour "hour")))
    ('day (format "~17@a ~a" (ls-make-colour colour (round->exact (/. (/. (/. time 60) 60) 24))) (ls-make-colour colour "day ")))
    ('month (format "~17@a ~a" (ls-make-colour colour (round->exact (/. (/. (/. (/. time 60) 60) 24) 30))) (ls-make-colour colour "mon ")))
    ('year (format "~17@a ~a" (ls-make-colour colour (round->exact (/. (/. (/. (/. (/. time 60) 60) 24) 30) 12))) (ls-make-colour colour "year")))
    ))

(define (print-time file stat)
  (let* ((curtime (sys-time))
        (file-time (file-ctime file))
        (delta (- curtime file-time)))
    (cond
      ;; sec
      ((< delta 10)
       (print-time-format 'sec 3 delta))
      ((< delta 60)
       (print-time-format 'sec 3 delta))
      ;; min
      ((< delta  (* 2 60))
       (print-time-format 'min 15 delta))
      ((< delta (* 45 60))
       (print-time-format 'min 15 delta))
      ;; hour
      ((< delta  (* 90 60))
       (print-time-format 'hour 9 delta))
      ((< delta (* 24 60 60))
       (print-time-format 'hour 9 delta))
      ((< delta (* 30 60 60))
       (print-time-format 'hour 9 delta))
      ((< delta (* 36 60 60))
       (print-time-format 'hour 9 delta))
      ;; day
      ((< delta  (* 48 60 60))
       (print-time-format 'day 4 delta))
      ((< delta (* 7 24 60 60))
       (print-time-format 'day 4 delta))
      ((< delta (* 14 24 60 60))
       (print-time-format 'day 4 delta))
      ((< delta (* 28 24 60 60))
       (print-time-format 'day 4 delta))
      ((< delta (* 30 24 60 60))
       (print-time-format 'day 4 delta))
      ;; month
      ((< delta (* 2 30 24 60 60))
       (print-time-format 'month 14 delta))
      ((< delta (* 12 30 24 60 60))
       (print-time-format 'month 14 delta))
      ;; year
      (else
       (print-time-format 'year 0 delta)))))

; }}}

(define (normal-files directory)
  (let ((dotfile (lambda (f) (rxmatch->string #/.*\/(\.)[^\/]*$/ f)))
        (files (list-files directory)))
    (remove dotfile files)))

(define (directory-first dirlist)
  (receive (dirs files)
    (partition
      file-is-directory?
      dirlist)
    (append dirs files)))



(define-syntax define-ls-proc
  (syntax-rules ()
    ((_ name ls directories allfiles dfirst)
     (define (name directories allfiles dfirst)
       (cond
         ((null? directories)
          (ls (current-directory) allfiles dfirst))
         (else
           (let loop ((dirs directories))
             (cond
               ((null? dirs)
                (values))
               (else
                 (ls (car dirs) allfiles dfirst)
                 (loop (cdr dirs)))))))))))


;; list files with permission, size, filename
(define ls-perm-size-file-proc
  (lambda (dir allfiles dfirst)
    (let ((fullpath-list (cond
                           ((and allfiles dfirst)
                            (directory-first  (list-files dir)))
                           (allfiles (list-files dir))
                           (dfirst   (directory-first (normal-files dir)))
                           (else (normal-files dir)))))
      (for-each
        (lambda (e) (display e) (newline))
        (map (lambda (f)
               (let1 stat (sys-lstat f)
                 (format "~a~10a~a~a~a~a"
                         (print-delimiter 1)
                         (print-permission f stat)
                         (print-delimiter 2)
                         (print-size f stat)
                         (print-delimiter 3)
                         (print-filename f stat))))
             fullpath-list)))))
(define-ls-proc ls-perm-size-file
                  ls-perm-size-file-proc
                  directories allfiles dfirst)

;; list files with permission, filename
(define ls-perm-file-proc
  (lambda (dir allfiles dfirst)
    (for-each
      (lambda (e) (display e) (newline))
      (map (lambda (f)
             (let1 stat (sys-lstat f)
               (format "~1a~10a~a~a"
                       (print-delimiter 1)
                       (print-permission f stat)
                       (print-delimiter 2)
                       (print-filename f stat))))
           (if allfiles
             (list-files dir)
             (normal-files dir))))))
(define-ls-proc ls-perm-file
                  ls-perm-file-proc
                  directories allfiles dfirst)

;; list files with permission, size, filename
(define ls-perm-owner-file-proc
  (lambda (dir allfiles dfirst)
    (for-each
      (lambda (e) (display e) (newline))
      (map (lambda (f)
             (let1 stat (sys-lstat f)
               (format "~1a~10a~a~a~a~a"
                       (print-delimiter 1)
                       (print-permission f stat)
                       (print-delimiter 2)
                       (print-owner f stat)
                       (print-delimiter 3)
                       (print-filename f stat))))
           (if allfiles
             (list-files dir)
             (normal-files dir))))))
(define-ls-proc ls-perm-owner-file
                  ls-perm-owner-file-proc
                  directories allfiles dfirst)

;; list files with permission, owner, size, filename
(define ls-perm-owner-size-file-proc
  (lambda (dir allfiles dfirst)
    (let ((fullpath-list (cond
                           ((and allfiles dfirst)
                            (directory-first  (list-files dir)))
                           (allfiles (list-files dir))
                           (dfirst   (directory-first (normal-files dir)))
                           (else (normal-files dir)))))
      (for-each
        (lambda (e) (display e) (newline))
        (map (lambda (f)
               (let1 stat (sys-lstat f)
                 (format "~a~10a~a~a~a~a~a~a"
                         (print-delimiter 1)
                         (print-permission f stat)
                         (print-delimiter 2)
                         (print-owner f stat)
                         (print-delimiter 3)
                         (print-size f stat)
                         (print-delimiter 3)
                         (print-filename f stat))))
             fullpath-list)))))
(define-ls-proc ls-perm-owner-size-file
                  ls-perm-owner-size-file-proc
                  directories allfiles dfirst)

;; list files with permission, filename
(define ls-perm-time-file-proc
  (lambda (dir allfiles dfirst)
    (for-each
      (lambda (e) (display e) (newline))
      (map (lambda (f)
             (let1 stat (sys-lstat f)
               (format "~1a~10a~a~a~a~a"
                       (print-delimiter 1)
                       (print-permission f stat)
                       (print-delimiter 2)
                       (print-time f stat)
                       (print-delimiter 3)
                       (print-filename f stat))))
           (if allfiles
             (list-files dir)
             (normal-files dir))))))
(define-ls-proc ls-perm-time-file
                  ls-perm-time-file-proc
                  directories allfiles dfirst)

;; list files with permission, time, size, filename
(define ls-perm-time-size-file-proc
  (lambda (dir allfiles dfirst)
    (let ((fullpath-list (cond
                           ((and allfiles dfirst)
                            (directory-first  (list-files dir)))
                           (allfiles (list-files dir))
                           (dfirst   (directory-first (normal-files dir)))
                           (else (normal-files dir)))))
      (for-each
        (lambda (e) (display e) (newline))
        (map (lambda (f)
               (let1 stat (sys-lstat f)
                 (format "~a~10a~a~a~a~a~a~a"
                         (print-delimiter 1)
                         (print-permission f stat)
                         (print-delimiter 2)
                         (print-time f stat)
                         (print-delimiter 3)
                         (print-size f stat)
                         (print-delimiter 3)
                         (print-filename f stat))))
             fullpath-list)))))
(define-ls-proc ls-perm-time-size-file
                  ls-perm-time-size-file-proc
                  directories allfiles dfirst)

(define  ls-perm-owner-time-size-file-proc
  (lambda (dir allfiles dfirst)
    (let ((fullpath-list (cond
                           ((and allfiles dfirst)
                            (directory-first  (list-files dir)))
                           (allfiles (list-files dir))
                           (dfirst   (directory-first (normal-files dir)))
                           (else (normal-files dir)))))
      (for-each
        (lambda (e) (display e) (newline))
        (map (lambda (f)
               (let1 stat (sys-lstat f)
                 (format "~a~10a~a~a~a~a~a~a~a~a"
                         (print-delimiter 1)
                         (print-permission f stat)
                         (print-delimiter 2)
                         (print-owner f stat)
                         (print-delimiter 3)
                         (print-time f stat)
                         (print-delimiter 3)
                         (print-size f stat)
                         (print-delimiter 3)
                         (print-filename f stat))))
             fullpath-list)))))
(define-ls-proc ls-perm-owner-time-size-file
                  ls-perm-owner-time-size-file-proc
                  directories allfiles dfirst)

;
(define (ls-file directories allfiles dfirst)
  (cond
    ((null? directories)
     (printcol (current-directory) allfiles dfirst))
    (else
      (let loop ((dirs  directories))
        (cond
          ((null? dirs)
           (read-from-string "")) ;return EOF
          (else
            (printcol (car dirs) allfiles dfirst)
            (loop (cdr dirs))))))))

(define (print-filename-col filename stat)
  (let*  ((file (sys-basename filename))
          (type (ref stat 'type))
          (extension  (path-extension file)))
    (cond
      (extension
        (if-let1 e (ref  *extension-colours* (string->symbol extension) #f)
          (colour-filename file type e)
          (colour-filename file type)))
      (else
        (colour-filename file type)))))

(define (printcol directory allfiles dfirst)
  (let ((currentlist (list-files directory)))
    (cond
      ((null? currentlist)
       #t)
      (else
        (let* ((tabwidth 2)
               (termwidth (string->number (process-output->string '(tput cols))))
               (maxwidth (cond
                           (allfiles
                             (logand (+ (apply max (map string-length (map sys-basename currentlist)))
                                        tabwidth) (lognot (- tabwidth 1))))
                           (else
                             (logand (+ (apply max (map string-length (map sys-basename (normal-files directory))))
                                        tabwidth) (lognot (- tabwidth 1))))))
               (num (cond
                      (allfiles
                        (length currentlist))
                      (else
                        (length (normal-files directory)))))
               (fullpath-list (cond ((and allfiles dfirst)
                                     (directory-first  currentlist))
                                (allfiles currentlist)
                                (dfirst   (directory-first (normal-files directory)))
                                (else (normal-files directory)))))
          (cond
            ((< termwidth (* 2 maxwidth))
             (for-each
               (lambda (e) (display e) (newline))
               (map (lambda (f)
                      (let1 stat (sys-stat f)
                        (format "~a" (print-filename-col f stat))))
                    fullpath-list)))
            (else
              (let*  ((numcols  (round->exact (/. termwidth maxwidth)))
                      (numrows  (round->exact (/. num numcols)))
                      (numrows-new (if  (< 0 (modulo num numcols))
                                     (+  numrows 1)
                                     numrows))
                      (lst fullpath-list)
                      (col (round->exact (/. termwidth numcols))))
                (let loop ((l (filter string? (take* lst numcols #t)))
                           (lst (filter string? (drop* lst numcols))))
                  (cond
                    ((null? l)
                     #t)
                    (else
                      (for-each
                        (lambda (f)
                          (let1 stat (sys-stat f)
                            (display (format "~a\t"  (print-filename-col f stat)))))
                        l)
                      (newline)
                      (loop (take* lst numcols ) (drop* lst numcols)))))))))))))

(define (usage)
  (print "help"))

(define (ls args)
  (let-args args
    ((pf "pf|perm-file")
     (ptf "ptf|perm-time-file")
     (ptsf "ptsf|perm-time-size-file")
     (pof "pof|perm-owner-file")
     (psf "psf|perm-size-file")
     (posf "posf|perm-owner-size-file")
     (potsf "potsf|perm-owner-time-size-file")
     (all "a|all")
     (dfirst "d|directory-first")
     . directories)
    (cond
      (pf (ls-perm-file directories all dfirst))
      (ptf (ls-perm-time-file directories all dfirst))
      (ptsf (ls-perm-time-size-file directories all dfirst))
      (pof (ls-perm-owner-file directories all dfirst))
      (posf (ls-perm-owner-size-file directories all dfirst))
      (potsf (ls-perm-owner-time-size-file directories all dfirst))
      (psf (ls-perm-size-file directories all dfirst))
      (dfirst (ls-file directories all dfirst))
      (else (ls-file directories all dfirst))))
  0)
