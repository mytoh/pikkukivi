":"; exec gosh -- $0 "$@"

(use gauche.process)
(use kirjasto)

(define (title)
  (run-process '(tput sc) :wait #true)
  ;; (run-process '(tput clear) :wait #true)
  (run-process '(tput civis) :wait #true)
  (run-process '(tput dl1) :wait #true)
  (run-process '(tput cup 0 2) :wait #true)
  (display
      (colour-string 160
                     (car (sys-uname))))
  (display "pidän kainalosta")
  (newline)
  (run-process '(tput rc) :wait #true))

(define (main args)
  (while #true
         (title)
         (run-process '(sleep 1))))
