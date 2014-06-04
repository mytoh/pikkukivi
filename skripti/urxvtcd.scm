":"; exec gosh -- $0 "$@"

(use gauche.process)
(use gauche.parseopt)
(use util.match)

(define (urxvtcd)
  (run-process '(urxvtd -q -o -f) :wait #true)
  (run-process '(urxvtc) :wait #true
               :detached #true
               :error  :null
               :output :null))

(define (main args)
  (urxvtcd))
