(define-module (png fsm chunk-context)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png fsm context)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:re-export (guard:#t
               action:no-op)
  #:export (<chunk-context>
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



(define-class <chunk-context> ()
  ;; Port to read the chunk data from.
  ;;
  ;; <port>
  (port
   #:init-keyword #:port
   #:getter       fsm-chunk-context-port)

  ;; Buffer to store the temporary data.
  ;;
  ;; <bytevector>
  (buffer
   #:init-thunk   (lambda () (make-bytevector %png-chunk-length-bytes 0))
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

(define-method (%buffer-reset! (ctx <chunk-context>) (bytes <number>))
  (fsm-chunk-context-buffer-index-set! ctx 0)
  (fsm-chunk-context-buffer-set! ctx (make-bytevector bytes 0)))


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
  (equal? (png-chunk-type (fsm-chunk-context-chunk ctx))
          'IEND))

(define (guard:data-read? ctx event)
  (let ((chunk (fsm-chunk-context-chunk ctx)))
    (= (- (png-chunk-length chunk) 1)
       (fsm-chunk-context-buffer-index ctx))))

(define (guard:crc-read? ctx event)
  (= (fsm-chunk-context-buffer-index ctx) 3))


;; Actions.

(define (action:store ctx byte)
  (let ((buf (fsm-chunk-context-buffer ctx)))
    (bytevector-u8-set! buf (fsm-chunk-context-buffer-index ctx) byte)
    (%buffer-index++! ctx)
    ctx))

(define (action:store-length ctx byte)
  (action:store ctx byte)
  (let ((chunk        (fsm-chunk-context-chunk ctx))
        (data         (fsm-chunk-context-buffer ctx)))
    (png-chunk-length-set! chunk (vector->int32 data))
    (%buffer-reset! ctx %png-chunk-type-bytes)
    ctx))

(define (action:store-type ctx byte)
  (action:store ctx byte)
  (let ((chunk (fsm-chunk-context-chunk ctx))
        (data  (fsm-chunk-context-buffer ctx)))
    (png-chunk-type-set! chunk (car (vector->chunk-type data)))
    (%buffer-reset! ctx (png-chunk-length chunk)))
  ctx)

(define (action:store-crc ctx byte)
  (action:store ctx byte)
  (let ((chunk (fsm-chunk-context-chunk ctx))
        (data  (fsm-chunk-context-buffer ctx)))
    (%png-chunk-crc-set! chunk (vector->int32 data))
    (%buffer-reset! ctx (png-chunk-length chunk)))
  ctx)

(define (action:store-data ctx byte)
  (unless (zero? (bytevector-length (fsm-chunk-context-buffer ctx)))
    (action:store ctx byte)
    (let ((chunk (fsm-chunk-context-chunk ctx))
          (data  (fsm-chunk-context-buffer ctx)))
      (png-chunk-data-set! chunk data)
      (%buffer-reset! ctx %png-chunk-crc-bytes)))
  ctx)
