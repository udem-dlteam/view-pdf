(define-library (view-pdf)

  (export view-pdf)

  (import (gambit))

  (begin

    (define (could-not-find-pdf-viewer)
      (error "a PDF viewer could not be found"))

    (define (open-using-first-available path programs)
      (call-with-current-continuation
       (lambda (return)
         (for-each (lambda (program)
                     (let ((r (shell-command
                               (string-append
                                program
                                " "
                                (object->string path))
                               #t)))
                       (if (= 0 (car r)) (return #t))))
                   programs)
         #f)))

    (define (view-pdf-fallback path)
      (or (open-using-first-available path
                                      '("evince"
                                       "okular"
                                       "qpdfview"
                                       "xdg-open"
                                       "epdfview"))
          (could-not-find-pdf-viewer)))

    (define (view-pdf-linux path)
      (view-pdf-fallback path))

    (define (view-pdf-macos path)
      (or (open-using-first-available path
                                      '("open"))
          (view-pdf-fallback path)))

    (define (view-pdf-windows path)
      (or (let ((r (shell-command
                    path
                    #t)))
            (= 0 (car r)))
          (view-pdf-fallback path)))

    (define (view-pdf path)
      (let ((os (symbol->string (caddr (system-type)))))
        (cond ((##string-prefix=? os "linux")
               (view-pdf-linux (path-expand path)))
              ((##string-prefix=? os "darwin")
               (view-pdf-macos (path-expand path)))
              ((or (##string-prefix=? os "windows")
                   (##string-prefix=? os "msys"))
               (view-pdf-windows (path-expand path)))
              (else
               (error "view-pdf does not know the OS" os)))))))
