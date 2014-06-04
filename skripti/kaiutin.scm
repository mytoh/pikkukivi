":"; exec gosh -- <|0|> "$@"
;; -*- coding: utf-8 -*-

(use text.tree)

(define (famima)
  (tree->string
   '("o4" "l8" "F#" "D" "<A>" "D" "E" ">A<" "P8." "<A>" "E" "F#" "E" "<A>" "D2"))
  ;; fa re "la" re  mi 'la' - "la"  mi fa mi "la" re -
  )

(define (marisa)
  ;; "http://toro.2ch.net/test/read.cgi/unix/1012808941"
  "T155MLL16E-FG-2.P8FG-L2A->D-<B-1PL4B->D-E-2E-G-F.D-.<A-G-.A-.B-F2L8F.G-.A-G-2.P8L16FG-A-2>D-2E-1L8PG-F4E-.<B-.>D-L4D-.<B.A->D.E-.FF.E-2"
  )

(define (jishin)
  ;; "http://toro.2ch.net/test/read.cgi/unix/1012808941"
  "T160MLO4L32G>CEA#>D#8<<G#>C#FB>E8<<P2G>CEA#>D#8<<G#>C#FB>E8<<P2")


(define (kaiutin notes)
  (call-with-output-file
      "/dev/speaker"
    (lambda (in)
      (display (notes)
        in)))
  )

(define (main args)
  (kaiutin famima)
  )
