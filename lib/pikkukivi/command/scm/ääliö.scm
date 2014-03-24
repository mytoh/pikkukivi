
(define-library (pikkukivi command scm ääliö)
    (export
      ääliö)
  (import
    (scheme base)
    (scheme write)
    (gauche base)
    (gauche process) ; run-process
    (gauche parseopt)
    (util match)
    (file util) ; directory-list, current-directory
    (maali)
    (clojure)
    (kirjasto tiedosto))

  (begin

    (define-constant *gitdir*  (expand-path "~/huone/git/"))

    (define-constant *repos*
      '(; normal repo
        "git://gitorious.org/cmus/cmus.git"
        "git://git.sv.gnu.org/screen.git"
        "git://git.savannah.gnu.org/stow.git"
        "git://git.infradead.org/get_iplayer.git"
        "git://git.mplayer2.org/mplayer2.git"
        "git://git.informatik.uni-erlangen.de/re06huxa/herbstluftwm"
        "git://opensource.conformal.com/xxxterm.git"
        "git://tron.homeunix.org/cvscvt"
        "git://pcmanfm.git.sourceforge.net/gitroot/pcmanfm/libfm"
        "git://pcmanfm.git.sourceforge.net/gitroot/pcmanfm/pcmanfm"
        ;; repo with other name
        ("git://git.sourceforge.jp/gitroot/ninix-aya/master.git"  "ninix-aya")
        ;; github repo
        (okuoku        mosh)
        (tmbinc        bgrep)
        (ninjaaron     bitocra)
        (koron         chalice)
        (rossy2401     img2xterm)
        (Arrowmaster   mcomix)
        (robbyrussell  oh-my-zsh)
        (sorin-ionescu prezto)
        (pkgng         pkgng)
        (coderholic    pyradio)
        (bmizerany     roundup)
        (muennich      sxiv)
        (alice0775     userChrome.js)
        (Griever       userChromeJS)
        (rkitover      vimpager)
        (GGLucas       vimperator-buftabs)
        (vimpr         vimperator-plugins)
        (caisui        vimperator)
        (clvv           fasd)
        (rupa           z)
        (rupa           v)
        (sjl            z-fish)
        (zsh-users     zsh-completions)
        (zsh-users     zsh-syntax-highlighting)
        (trapd00r      zsh-syntax-highlighting-filetypes)
        (trapd00r      ls--)
        (trapd00r      utils trapd00r-utils)
        (trapd00r      configs trapd00r-configs)
        (hchbaw        auto-fu.zsh)
        (buntine       Fractals)
        (SanskritFritz fish_completions)
        (esodax        fishystuff)
        (zmalltalker        fish-nuggets)
        (Nandaka       DanbooruDownloader)
        (frytvm        XS)
        (joelagnel     stumpwm-goodies)
        (sabetts       stumpwm)
        (aredridel      es-shell)
        (mooz           percol)
        (xorg62         wmfs)
        (shawncplus     ghub)
        (86me           pentadactyl-scripts)
        (lf94           dwm-lee)
        (Cloudef        monsterwm-xcb)
        (Cloudef        dotFiles   cloudef-dotFiles)
        (Cloudef        dotfiles-ng   cloudef-dotFiles-ng)
        (DaisukeAramaki Dotfiles)
        (chjj           compton)
        (samirahmed           fu)
        (defunkt        hub)
        (huyz           less.vim)
        (ivmai          bdwgc)
        (ivmai          libatomic_ops)
        (gmarik         vimfiles gmarik-vimfiles)
        (valvallow      lifegame)
        (saironiq       shellscripts)
        (mason-larobina luakit)
        (robm           dzen)
        (jhawthorn     meh)
        (kahua         Kahua)
        (digego         extempore)
        (tlatsas       xcolors)
        (Raynes       fs)
        (clojure      clojure-contrib)
        (clojure      core.logic)
        (sgrove       tehila)
        (tyru         dotto)
        (ThomasAdam     tmux)
        (erikw        tmux-powerline)
        (seebi        tmux-colors-solarized)
        (altercation        solarized)
        (cucumber        cucumber)
        (cavalle        steak)
        (cldwalker        boson-more)
        (cldwalker        boson)
        (webyrd        miniKanren)
        (chujoii       battery-scheme)
        (chujoii       xattr-tag)
        (ghc ghc)
        (francesco-bracchi sake)
        (adamv          homebrew)
        (okuoku r7rs-bridge)
        ))

    ;; update git repository
    (define (update-gitdir)
      (let ((dirs (list (directory-list (expand-path *gitdir*) :children? #true :add-path? #true))))
        (let loop ((dirs (car dirs)))
             (cond
               ((null? dirs)
                (display "update finished!\n"))
               (else
                   (when (file-is-directory? (car dirs))
                     (display (paint "=> " 4))
                     (display (paint (sys-basename (car dirs)) 3))
                     (newline)
                     (run-process '(git pull) :wait #true :directory (car dirs)))
                 (newline)
                 (loop (cdr dirs)))))))


    ;; clean git repository with "git gc"
    (define (clean-gitdir)
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


    ;; clone git repository
    (define (clone-gitdir)
      (let ((clone (lambda (url dirname) (run-process `(git clone ,url ,dirname) :wait #true))))
        (for-each
            (lambda (l)
              (if-not (file-is-directory? (x->string (car l)))
                      (clone (cadr l) (car l))
                      #true))
          (repo-url-directory-list))
        #true))

    (define (repo-url-directory-list)
      (map
          (lambda (e)
            (cond
              ((string? e)
                                        ; normal repo
               (list (sys-basename (path-sans-extension e)) e))
              ((list? e)
               (cond
                 ((string? (car e))
                                        ; normal repo
                  (list (cadr e) (car e)))
                 ((= (length e) 2)
                                        ; github
                  (list (cadr e) #`"git://github.com/,(car e)/,(cadr e)"))
                                        ; github with renaming
                 (else
                     (list (caddr e) #`"git://github.com/,(car e)/,(cadr e)"))))
              ((symbol? e)
                                        ; my github
               (list e #`"git@github.com:mytoh/,|e|"))
              (else
                  (list e))))
        *repos* ))

    (define (list-repos)
      (map
          (^ (x) (cond
                   ((list? x)
                    (print
                     (string-append (paint (x->string (car x)) 33)
                       ": "
                       (paint (x->string (cadr x)) 93))))
                   (else
                       (print x))))
        *repos*))

    (define (usage status)
      (exit status "usage: ~a <command> <package-name>\n" *program-name*))


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
                                    (update-gitdir)))
                                 ("clean"
                                  (clean-gitdir))
                                 ("list"
                                  (list-repos))
                                 ("clone"
                                  (begin
                                    (print (string-append (paint "cloning " 3) "repositories"))
                                    (clone-gitdir)))
                                 (_ (usage 1)))))
      0)
    ))
