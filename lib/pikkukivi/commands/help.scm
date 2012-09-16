
(define-module pikkukivi.commands.help
  (export
    help)
  (use gauche.process)
  (use util.list) ; slices
  (use util.match)
  (use file.util)
  (require-extension (srfi 1 13))    ; iota
  (use pikkukivi)
  )
(select-module pikkukivi.commands.help)

(define (help args)
  (exit 0
        (string-append
          "usage: panna <command> <package>\n"
          "\n"
          (make-help "commands"  "; list available commands\n")
          (make-help "help    "  "; display this message\n")
          )))

(define (make-help cmd mes)
  (format #f "    ~@a ~@a" (colour-string 5 cmd) (colour-string 223 mes)))

