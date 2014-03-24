;;; methods from
;;; cv2tar.scm by
;;;  Walter C. Pelissero

(define-library (pikkukivi command unpack)
    (export unpack)
  (import
    (scheme base)
    (gauche base)
    (gauche process)
    (file util) ; path-extension
    )
  (begin

    (define (run command)
      (run-process command :wait #true))

    (define (zip-unpacker file . directory)
      (if (null? (car directory))
        (run `(unzip -q ,file))
        (run `(unzip -q ,file -d ,(caar directory)))))


    (define (rar-unpacker file . directory)
      (cond ((null? (car directory))
             (run `(unrar x -ad -inul ,file)))
            (else
                (run `(unrar x -ad -inul ,file ,(caar directory))))))

    (define (lha-unpacker file . diretory)
      (run `(lha xq ,file)))

    (define (tar-unpacker file . directory)
      (cond ((null? (car directory))
             (run `(tar xf ,file)))
            (else
                (make-directory* (caar directory))
              (run `(tar xf ,file -C ,(caar directory))))))

    (define (sevenzip-unpacker file . directory)
      (if (null? (car directory))
        (run `(7z x ,file))
        (run `(7z x ,file -o ,(caar directory)))))

    (define-constant *unpacker-alist*
      `(
        ("zip" ,zip-unpacker)
        ("cbz" ,zip-unpacker)

        ("7z" ,sevenzip-unpacker)

        ("rar" ,rar-unpacker)
        ("cbr" ,rar-unpacker)

        ("lha" ,lha-unpacker)

        ("gz"  ,tar-unpacker)
        ("tgz" ,tar-unpacker)
        ("bz2" ,tar-unpacker)
        ("xz"  ,tar-unpacker)
        ("txz" ,tar-unpacker)
        ("cbx" ,tar-unpacker)
        ("tar" ,tar-unpacker)))

    (define (unpacker file . directory)
      (let ((unpacker (cadr (assoc (path-extension file)
                              *unpacker-alist*))))
        (print file)
        (if unpacker
          (if directory
            (unpacker file directory)
            (unpacker file))
          (error "unknown file type" file))))

    (define (unpack args)
      (if (<= 2 (length args))
        (unpacker (car args) (cadr args))
        (unpacker (car args))))))
