;;; spin.scm

 (define-library (pikkukivi command spin main)

    (export spin)

  (import (scheme base)
          (scheme write)
          (kirjasto pääte)
          (gauche base)
          )

  (begin

    (define (gen-list lst)
      (let ((lst-max (- (length lst) 1))
            (index 0))
        (lambda ()
          (cond
            ((eq? index lst-max)
             (let ((ret (list-ref lst index)))
               (set! index 0)
               ret))
            (else
                (let ((ret (list-ref lst index)))
                  (set! index (+ index 1))
                  ret))))))

    (define gen-spin
      (gen-list '("|" "/" "-" "\\")))

    (define (do-spin)
      (let loop ()
           (tput-clr-bol)
           (display (gen-spin))
           (flush)
           (sys-select #false #false #false 100000)
           (display "\r")
           (tput-clr-eol)
           (loop)))


    (define (spin args)
      (tput-cursor-invisible)
      (do-spin)
      (tput-cursor-normal))

    ))
