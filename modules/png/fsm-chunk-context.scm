(define-module (png fsm-chunk-context)
  #:use-module (ice-9 binary-ports)
  #:use-module (oop goops)
  #:use-module (smc context context)
  #:use-module (png core chunk)
  #:re-export (guard:#t
               action:no-op)
  #:export (<fsm-chunk-context>
            event-source
            event-source:no-op
            fsm-chunk-context-chunk
            guard:length-read?
            guard:type-read?
            guard:data-read?
            guard:crc-read?
            guard:iend-chunk?
            action:store
            action:store-length
            action:store-type
            action:store-crc
            action:store-data))



(define-class <fsm-chunk-context> ()
  (port
   #:init-keyword #:port
   #:getter       fsm-chunk-context-port)

  ;; <vector>
  (buffer
   #:init-thunk   (lambda () (make-vector %png-chunk-length-bytes))
   #:getter       fsm-chunk-context-buffer
   #:setter       fsm-chunk-context-buffer-set!)

  ;; <number>
  (buffer-index
   #:init-value   0
   #:getter       fsm-chunk-context-buffer-index
   #:setter       fsm-chunk-context-buffer-index-set!)

  (chunk
   #:init-thunk   (lambda () (make <png-chunk>))
   #:getter       fsm-chunk-context-chunk))



(define-method (%buffer-index++! ctx)
  (fsm-chunk-context-buffer-index-set! ctx
                                       (+ (fsm-chunk-context-buffer-index ctx) 1)))

(define-method (%buffer-reset! (ctx <fsm-chunk-context>) (bytes <number>))
  (fsm-chunk-context-buffer-index-set! ctx 0)
  (fsm-chunk-context-buffer-set! ctx (make-vector bytes)))

(define-method (%buffer->number (buffer <vector>))
  (logior (ash (vector-ref buffer 0) 24)
          (ash (vector-ref buffer 1) 16)
          (ash (vector-ref buffer 2) 8)
          (vector-ref buffer 3)))


;; Event source.
(define (event-source ctx)
  (get-u8 (fsm-chunk-context-port ctx)))

(define (event-source:no-op ctx)
  #t)


;; Guards.
(define (guard:length-read? ctx event)
  (= (fsm-chunk-context-buffer-index ctx) 3))

(define (guard:type-read? ctx event)
  (= (fsm-chunk-context-buffer-index ctx) 3))

(define (guard:iend-chunk? ctx event)
  (equal? (png-chunk-type/name (fsm-chunk-context-chunk ctx))
          'IEND))

(define (guard:data-read? ctx event)
  (let ((chunk (fsm-chunk-context-chunk ctx)))
    (= (- (png-chunk-length chunk) 1)
       (fsm-chunk-context-buffer-index ctx))))

(define (guard:crc-read? ctx event)
  (= (fsm-chunk-context-buffer-index ctx) 3))


;; Actions.

(define (action:store ctx byte)
  ;; (format (current-error-port) "byte: ~a~%" byte)
  (let ((buf (fsm-chunk-context-buffer ctx)))
    (vector-set! buf (fsm-chunk-context-buffer-index ctx) byte)
    (%buffer-index++! ctx)
    ctx))

(define (action:store-length ctx byte)
  (action:store ctx byte)
  (let ((chunk (fsm-chunk-context-chunk ctx))
        (data  (fsm-chunk-context-buffer ctx)))
    (png-chunk-length-set! chunk (%buffer->number data))
    (%buffer-reset! ctx %png-chunk-type-bytes)
    ctx))

(define (action:store-type ctx byte)
  (action:store ctx byte)
  (let ((chunk (fsm-chunk-context-chunk ctx))
        (data  (fsm-chunk-context-buffer ctx)))
    (png-chunk-type-set! chunk data)
    ;; (format (current-error-port) "action:store-type: chunk type (raw): ~a~%"
    ;;         data)
    ;; (format (current-error-port) "action:store-type: chunk type: ~a~%"
    ;;         (vector->chunk-type (png-chunk-type chunk)))
    (%buffer-reset! ctx (png-chunk-length chunk)))
  ctx)

(define (action:store-crc ctx byte)
  (action:store ctx byte)
  (let ((chunk (fsm-chunk-context-chunk ctx))
        (data  (fsm-chunk-context-buffer ctx)))
    (png-chunk-crc-set! chunk data)
    (%buffer-reset! ctx (png-chunk-length chunk)))
  ctx)

(define (action:store-data ctx byte)
  (unless (zero? (vector-length (fsm-chunk-context-buffer ctx)))
    (action:store ctx byte)
    (let ((chunk (fsm-chunk-context-chunk ctx))
          (data  (fsm-chunk-context-buffer ctx)))
      ;; (format (current-error-port) "store-data: ~a~%" data)
      (png-chunk-data-set! chunk data)
      (%buffer-reset! ctx %png-chunk-crc-bytes)))
  ctx)
