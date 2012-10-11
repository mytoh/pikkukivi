":"; exec gosh -- $0 "$@"
;; -*- coding: utf-8 -*-

(define-module pikkukivi.commands.tmux-start
  (export tmux-start)
  (use gauche.process)
  (use gauche.parseopt)
  (use util.match)
  (use srfi-98))
(select-module pikkukivi.commands.tmux-start)


(define (new-session session window-name . command)
  (when (not (has-session? session))
    (run-process `(tmux -u2 new-session -d -s ,session -n ,window-name ,@command) :wait #t)))

(define (new-window session window-name . command)
  (run-process `(tmux -u2 new-window
                      -d
                      -n ,window-name ,@command) :wait #t))

(define (attach-session session)
  (run-process `(tmux -u2 attach-session -t ,session) :wait #t))

(define  (has-session? session)
  (let* ((p (run-process `(tmux -u2 -q has-session -t ,session) :wait #t :output :null :error :null))
         (status (process-exit-status p)))
    (if (zero? status)
      #t
      #f)))

(define (tmux)
  (cond
    ; inside tmux
    ((sys-getenv "TMUX")
     (print "[38;5;1myou're inside of tmux[0m"))
    (else
      (let ((main-session   "main")
            (second-session "daemon")
            (third-session  "servers")
            (shell (sys-getenv "SHELL")))
        (cond
          ; session exists
          ((has-session? main-session)
           (attach-session main-session))
          ; session not exists
          (else
            ;; create main session
            (new-session main-session "main")
            (new-window main-session  "vim" "vim")
            (new-window main-session  "w3m" "w3m google.com")

            ;; create second session
            (new-session second-session "futaba" )
            (new-window  second-session  "4ch" shell)
            (new-window  second-session  "danbooru" shell)
            (new-window  second-session  "konachan" shell)
            (new-window  second-session  "rtorrent" shell)
            ; (new-window  second-session "rtorrent" "rtorrent")

            ;; create third session
            (new-session third-session "servers")
            (new-window  third-session  "2ch" "ssh mona@2ch.homelinux.org")

            ; attach main session
            (attach-session main-session)))))))

(define (tmux-start args)
  (tmux))
