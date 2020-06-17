(define-library (view-pdf)

  (export view-pdf)

  (import (gambit))

  (begin

    (define (could-not-find-pdf-viewer)
      (error "a PDF viewer could not be found"))

    (define (run-using-first-available programs path)
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
      (or (run-using-first-available '("evince" "okular" "qpdfview")
                                     path)
          (could-not-find-pdf-viewer)))

    (define (view-pdf-linux path)
      (view-pdf-fallback path))

    (define (view-pdf-macos path)
      (or (let ((r (shell-command
                    (string-append
                     "osascript -e \"tell application \\\"Preview\\\"\" -e \"open "
                     (object->string (object->string path))
                     "\" -e \"end tell\"")
                    #t)))
            (= 0 (car r)))
          (view-pdf-fallback path)))

    (define (view-pdf-windows path)
      (or (let ((r (shell-command
                    (object->string path)
                    #t)))
            (= 0 (car r)))
          (view-pdf-fallback path)))

    (define (view-pdf path)
      (let ((os (symbol->string (caddr (system-type)))))
        (cond ((##string-prefix=? os "linux")
               (view-pdf-linux (path-expand path)))
              ((##string-prefix=? os "darwin")
               (view-pdf-macos (path-expand path)))
              ((##string-prefix=? os "windows")
               (view-pdf-windows (path-expand path)))
              (else
               (error "view-pdf does not know the OS" os)))))))
