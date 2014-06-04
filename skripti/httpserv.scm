
(use gauche.net)
(use file.util)

(define sock #false)

(define (server-start)
  (make-server-socket 8888 :reuse-addr? #true))

(define (server-accept sock content)
  (let ((soc (socket-accept sock)))
    (while #true
           (socket-send soc (string-append "HTTP/1.1 200 OK\ncontent-type: text/html;\n\n" content))
           (socket-close soc)
           (set! soc (socket-accept sock))
           )))

(define (main arg)
  (let ((server (server-start))
        (file (port->string (open-input-file (string-append (home-directory) "/.site/index.html")))))
    (server-accept server file)))
