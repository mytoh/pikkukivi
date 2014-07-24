;; -*- coding: utf-8 -*-

(define-library (pikkukivi command tmux-start)
    (export tmux-start)
  (import
    (scheme base)
    (gauche)
    (gauche process)
    (gauche parseopt)
    (util match)
    (file util)
    (srfi 98))

  (begin

    (define (new-session session window-name . command)
      (when (not (has-session? session))
        (run-process `(tmux -u2 new-session -d -s ,session -n ,window-name ,@command) :wait #true)))

    (define (new-window session window-name . command)
      (run-process `(tmux -u2 new-window
                          -d
                          -n ,window-name ,@command) :wait #true))

    (define (attach-session session)
      (run-process `(tmux -u2 attach-session -t ,session) :wait #true))

    (define (has-session? session)
      (let* ((p (run-process `(tmux -u2 -q has-session -t ,session) :wait #true :output :null :error :null))
             (status (process-exit-status p)))
        (if (zero? status)
          #true
          #false)))

    (define (process-running? name)
      (if (equal? "" (process-output->string (string-append "pgrep " name) :on-abnormal-exit :ignore))
        #false #true))

    (define (ncmpcpp session)
      (cond
        ((and (find-file-in-paths "musicpd")
           (find-file-in-paths "ncmpcpp"))
         (if (not (process-running? "musicpd"))
           (new-window session "ncmpcpp" "musicpd && ncmpcpp")
           (new-window session "ncmpcpp" "ncmpcpp")))))

    (define (stat session)
      (cond
        ((find-file-in-paths "systat")
         (if (find-file-in-paths "slurm")
           (new-window session "stat" "slurm -i em0")
           (new-window session "stat"))
         (run-process '(tmux select-window -t "stat") :wait #true)
         (if (find-file-in-paths "htop")
           (run-process '(tmux split-window -h -t 0 "htop") :wait #true))
         (if (find-file-in-paths "tcpdump")
           (run-process '(tmux split-window -v -t 0 "sudo tcpdump -i em0 -s 0 -A port 80 | grep GET") :wait #true))
         (run-process '(tmux select-window -t "main") :wait #true)
         )))

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
                  (ncmpcpp main-session)
                  (stat main-session)

                  ;; create second session
                  (new-session second-session "futaba")
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

    ))
