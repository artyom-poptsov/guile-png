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
      (make <png-image>
        #:chunks (if raw?
                     (reverse (png-context-chunks context))
                     (map png-chunk->typed-chunk
                          (reverse (png-context-chunks context))))))))

;;; png.scm ends here.
