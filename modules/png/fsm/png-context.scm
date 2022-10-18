(define-module (png fsm png-context)
  #:use-module (ice-9 binary-ports)
  #:use-module (oop goops)
  #:use-module (smc context context)
  #:use-module (smc fsm)
  #:use-module (png fsm signature-parser)
  #:use-module (png fsm signature-context)
  #:use-module (png fsm chunk-context)
  #:use-module (png fsm chunk-parser)
  #:use-module (png core chunk)
  #:use-module (png image)
  #:re-export (guard:#t
               action:no-op)
  #:export (<png-context>
            read-header
            read-chunk
            store-chunk
            event-source

            ;; Guards
            guard:context-has-errors?
            guard:known-chunk?
            guard:iend-chunk?

            header-error
            chunk-type-error
            chunk-error
            png-context-chunks))



(define-class <png-context> ()
  ;; <port>
  (port
   #:init-keyword #:port
   #:getter       png-context-port)

  ;; <boolean>
  (debug-mode?
   #:init-value   #f
   #:init-keyword #:debug-mode?
   #:getter       png-context-debug-mode?)

  ;; <chunk> | #f
  (current-chunk
   #:init-value   #f
   #:getter       png-context-current-chunk
   #:setter       png-context-current-chunk-set!)

  (chunks
   #:init-value  '()
   #:getter      png-context-chunks
   #:setter      png-context-chunks-set!)

  ;; <list>
  (errors
   #:init-value   '()
   #:getter       png-context-errors
   #:setter       png-context-errors-set!))

(define-method (png-context-errors-add! (ctx     <png-context>)
                                        (key     <symbol>)
                                        (args    <list>))
  (png-context-errors-set! ctx
                           (cons (png-context-errors ctx)
                                 (list key args))))

(define-method (png-context-has-errors? (ctx <png-context>))
  (not (null? (png-context-errors ctx))))

(define-method (png-context-chunks-add! (ctx <png-context>) (chunk <png-chunk>))
  (png-context-chunks-set! ctx
                           (cons chunk (png-context-chunks ctx))))


;; Event sources.

(define (event-source ctx)
  #t)

(define-method (read-header (ctx <png-context>))
  (let ((fsm (make <signature-parser>)))
    (catch #t
      (lambda ()
        (fsm-run! fsm (make <signature-context>
                        #:port        (png-context-port ctx)
                        #:debug-mode? (png-context-debug-mode? ctx))))
      (lambda (key . args)
        (png-context-errors-add! ctx key args)))
    ctx))

(define-method (read-chunk (ctx <png-context>))
  (let ((fsm (make <chunk-parser>)))
    (catch #t
      (lambda ()
        (let ((chunk-ctx (fsm-run! fsm (make <chunk-context>
                                         #:port        (png-context-port ctx)
                                         #:debug-mode? (png-context-debug-mode? ctx)))))
          (png-context-current-chunk-set! ctx (fsm-chunk-context-chunk chunk-ctx))))
      (lambda (key . args)
        (png-context-errors-add! ctx key args)))
    ctx))

(define-method (store-chunk (ctx <png-context>))
  (png-context-chunks-add! ctx (png-context-current-chunk ctx))
  ctx)


;; Guards.

(define (guard:context-has-errors? ctx event)
  (png-context-has-errors? ctx))

(define (guard:known-chunk? ctx event)
  (let ((chunk (png-context-current-chunk ctx)))
    (png-chunk-type chunk)))

(define (guard:iend-chunk? ctx chunk-context)
  (let ((chunk (png-context-current-chunk ctx)))
    (equal? (png-chunk-type/name chunk) 'IEND)))



(define-method (header-error (ctx <png-context>))
  (error "PNG header error" (png-context-errors ctx)))

(define-method (chunk-error (ctx <png-context>))
  (error "Chunk error" (png-context-errors ctx)))

(define-method (chunk-type-error (ctx <png-context>))
  (error "Wrong chunk type"
         (png-context-current-chunk ctx)
         (png-context-errors ctx)))

;;; fsm-context.scm ends here.
