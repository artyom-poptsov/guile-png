(define-module (png core chunk plte)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:PLTE>
            png-chunk:PLTE-palette-entries-count
            png-chunk:PLTE-palette-entries
            png-chunk:PLTE-palette-entry
            palette:red
            palette:green
            palette:blue
            vector->PLTE-palette-entries
            data->png-chunk:PLTE))



(define-class <png-chunk:PLTE> (<png-chunk>)
  ;; This field contains a palette entries, each a three-byte vector of the form:
  ;;
  ;;   #vu8(red green blue)
  ;;
  ;; <vector> of <bytevector>
  (palette-entries
   #:init-thunk   (lambda () (make-vector 0))
   #:init-keyword #:palette-entries
   #:getter       png-chunk:PLTE-palette-entries))

(define-method (initialize (chunk <png-chunk:PLTE>) initargs)
  (next-method)
  (slot-set! chunk 'type 'PLTE))



(define-method (%display (chunk <png-chunk:PLTE>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:PLTE ~a (~a entr~:@p) ~a>"
            (list-ref type 2)
            (png-chunk:PLTE-palette-entries-count chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:PLTE>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:PLTE>) (port <port>))
  (%display chunk port))



(define-method (png-chunk:PLTE-palette-entry (chunk <png-chunk:PLTE>)
                                             (index <number>))
  (vector-ref (png-chunk:PLTE-palette-entries chunk) index))

(define-method (png-chunk:PLTE-palette-entries-count (chunk <png-chunk:PLTE>))
  (vector-length (png-chunk:PLTE-palette-entries chunk)))



(define-method (palette:red (plte <bytevector>))
  (bytevector-u8-ref plte 0))

(define-method (palette:green (plte <bytevector>))
  (bytevector-u8-ref plte 1))

(define-method (palette:blue (plte <bytevector>))
  (bytevector-u8-ref plte 2))



(define-method (vector->PLTE-palette-entries (vec <bytevector>))
  (let ((vlen (bytevector-length vec)))
    (let loop ((offset 0)
               (result '()))
      (if (< offset vlen)
          (loop (+ offset 3)
                (cons (u8-list->bytevector
                       (list (bytevector-u8-ref vec (+ 0 offset))
                             (bytevector-u8-ref vec (+ 1 offset))
                             (bytevector-u8-ref vec (+ 2 offset))))
                      result))
          (list->vector (reverse result))))))

(define-method (data->png-chunk:PLTE  (data   <bytevector>)
                                      (type   <symbol>)
                                      (length <number>)
                                      (crc    <number>))
  (unless (zero? (remainder (bytevector-length data) 3))
    (error "Invalid PLTE chunk: data length not divisible by 3" data))
  (make <png-chunk:PLTE>
    #:length             length
    #:type               type
    #:data               data
    #:crc                crc
    #:palette-entries    (vector->PLTE-palette-entries data)))

;;; chunk-plte.scm ends here.

