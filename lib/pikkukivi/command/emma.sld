
(define-library (pikkukivi command emma)
    (export emma)
  (import
    (scheme base)
    (gauche base)
    (gauche process)
    (kirjasto komento työkalu)
    (kirjasto väri)
    (kirjasto merkkijono)
    (srfi 1)
    (srfi  13)
    (srfi 27))
  (begin
    (define response
      '("muridana"
        "saaaaaanyaaaa"
        "X_X"
        ))
    (define (message str)
      (sys-sleep #e1e0)
      (if (string=? str "quit")
        (quit)
        (random-message)))

    (define (random-message)
      (print
       (ref response (random-integer (length response)))))

    (define (quit)
      (print "bye")
      (exit))

    ;; reader
    (define reader
      (lambda ()
        (let ((str (read-line)))
          str)))

    ;; evaluator
    (define evaluator
      (lambda (str env)
        str))

    ;; printer
    (define printer
      (lambda (str)
        (message str)))

    ;; prompter
    (define prompter
      (lambda ()
        (display (colour-string 33 "> "))
        (flush)))

    (define (emma args)
      (print "hello")
      (read-eval-print-loop
       reader
       evaluator
       printer
       prompter))
    ))
