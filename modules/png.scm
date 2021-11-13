(define-module (png)
  #:use-module (oop goops)
  #:use-module (smc core log)
  #:use-module (smc fsm)
  #:use-module (png image)
  #:use-module (png fsm-context)
  #:use-module (png fsm)
  #:use-module (png core chunk)
  #:use-module (png chunk-converter)
  #:export (png->scm))



(define* (png->scm port
                   #:key
                   (debug-mode? #f)
                   (raw?        #f))
  (let ((fsm (make <fsm-png>)))
    (fsm-debug-mode-set! fsm debug-mode?)
    (log-use-stderr! debug-mode?)
    (let ((context (fsm-run! fsm (make <png-context>
                                   #:debug-mode? debug-mode?
                                   #:port        port))))
      (let ((image (make <png-image>
                     #:chunks (reverse (png-context-chunks context)))))
        (if raw?
            image
            (make <png-image>
              #:chunks (map (lambda (chunk)
                              (png-chunk->typed-chunk image chunk))
                            (png-image-chunks image))))))))

;;; png.scm ends here.
