
(define-library (pikkukivi commands help)
    (export
      help)
  (import
    (scheme base)
    (gauche base)
    (gauche process)
    (util list) ; slices
    (util match)
    (file util)
    (srfi 1)
    (srfi  13)
    (maali)
    (pikkukivi))
  (begin
    (define (help args)
      (exit 0
        (string-append
            "usage: panna <command> <package>\n"
          "\n"
          (desc "commands"  "; list available commands\n")
          (desc "help    "  "; display this message\n"))))

    (define (desc cmd mes)
      (format #false "    ~@a ~@a" (paint cmd 5) (paint mes 223)))))
