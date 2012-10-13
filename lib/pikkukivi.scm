
(define-module pikkukivi
  (export colour-string)
  (use text.tree)
  )
(select-module pikkukivi)


(define (colour-string colour-number str)
  ;; take any -> return string
  (tree->string `("[38;5;" ,colour-number "m" ,str "[0m")))
