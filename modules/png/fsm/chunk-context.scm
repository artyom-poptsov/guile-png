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
            context-buffer-index
            context-buffer-index-set!
            event-source
            event-source:no-op
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



(define-class <chunk-context> (<port-context>)
  ;; <number>
  (buffer-index
   #:init-value   0
   #:getter       context-buffer-index
   #:setter       context-buffer-index-set!))

(define-method (initialize (context <chunk-context>) initargs)
  (next-method)
  (context-buffer-set! context (make-bytevector %png-chunk-length-bytes 0))
  (context-result-set! context (make <png-chunk>)))


(define fsm-chunk-context-port context-port)



(define-method (%buffer-index++! ctx)
  (context-buffer-index-set! ctx
                                       (+ (context-buffer-index ctx) 1)))

(define-method (%buffer-reset! (ctx <chunk-context>) (bytes <number>))
  (context-buffer-index-set! ctx 0)
  (context-buffer-set! ctx (make-bytevector bytes 0)))


;; Event source.
(define (event-source ctx)
  (get-u8 (fsm-chunk-context-port ctx)))

(define (event-source:no-op ctx)
  #t)


;; Guards.
(define (guard:length-read? ctx event)
  (= (context-buffer-index ctx) 3))

(define (guard:type-read? ctx event)
  (= (context-buffer-index ctx) 3))

(define (guard:iend-chunk? ctx event)
  (equal? (png-chunk-type (context-result ctx))
          'IEND))

(define (guard:data-read? ctx event)
  (let ((chunk (context-result ctx)))
    (= (- (png-chunk-length chunk) 1)
       (context-buffer-index ctx))))

(define (guard:crc-read? ctx event)
  (= (context-buffer-index ctx) 3))


;; Actions.

(define (action:store ctx byte)
  (let ((buf (context-buffer ctx)))
    (bytevector-u8-set! buf (context-buffer-index ctx) byte)
    (%buffer-index++! ctx)
    ctx))

(define (action:store-length ctx byte)
  (action:store ctx byte)
  (let ((chunk        (context-result ctx))
        (data         (context-buffer ctx)))
    (png-chunk-length-set! chunk (vector->int32 data))
    (%buffer-reset! ctx %png-chunk-type-bytes)
    ctx))

(define (action:store-type ctx byte)
  (action:store ctx byte)
  (let ((chunk (context-result ctx))
        (data  (context-buffer ctx)))
    (png-chunk-type-set! chunk (car (vector->chunk-type data)))
    (%buffer-reset! ctx (png-chunk-length chunk)))
  ctx)

(define (action:store-crc ctx byte)
  (action:store ctx byte)
  (let ((chunk (context-result ctx))
        (data  (context-buffer ctx)))
    (%png-chunk-crc-set! chunk (vector->int32 data))
    (%buffer-reset! ctx (png-chunk-length chunk)))
  ctx)

(define (action:store-data ctx byte)
  (unless (zero? (bytevector-length (context-buffer ctx)))
    (action:store ctx byte)
    (let ((chunk (context-result ctx))
          (data  (context-buffer ctx)))
      (png-chunk-data-set! chunk data)
      (%buffer-reset! ctx %png-chunk-crc-bytes)))
  ctx)
