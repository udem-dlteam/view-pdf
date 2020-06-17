(define-library (demo)

  (import (..view-pdf)
          (gambit))

  (begin

    (define (view-hello-gambit)
      (view-pdf
       (path-expand "hello-gambit.pdf"
                    (path-directory (this-source-file)))))

    (view-hello-gambit)))
