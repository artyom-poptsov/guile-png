#!/usr/bin/guile \
-L modules -e main -s
!#

(use-modules (oop goops)
             (smc fsm)
             (smc core log)
             (smc core state)
             (png fsm-context)
             (png fsm))

(define (main args)
  (let ((fsm (make <fsm-png>)))
    (fsm-debug-mode-set! fsm #t)
    (log-use-stderr! #t)
    (let ((context (fsm-run! fsm (make <png-context>
                                   #:debug-mode? #t
                                   #:port (current-input-port)))))
      (display "PNG chunks:\n")
      (for-each (lambda (chunk)
                  (format #t "  ~a~%" chunk))
                (reverse (png-context-chunks context))))))
