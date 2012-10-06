
(define-module pikkukivi.commands.help
  (export
    help)
  (use gauche.process)
  (use util.list) ; slices
  (use util.match)
  (use file.util)
  (require-extension (srfi 1 13))    ; iota
  (use maali)
  (use pikkukivi)
  )
(select-module pikkukivi.commands.help)

(define (help args)
  (exit 0
        (string-append
          "usage: panna <command> <package>\n"
          "\n"
          (desc "commands"  "; list available commands\n")
          (desc "help    "  "; display this message\n")
          )))

(define (desc cmd mes)
  (format #f "    ~@a ~@a" (paint cmd 5 ) (paint mes 223 )))

