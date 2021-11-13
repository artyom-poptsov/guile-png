(define-module (png core chunk-bkgd)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:bKGD>
            png-chunk:bKGD-colour-type
            png-chunk:bKGD-grayscale
            png-chunk:bKGD-red
            png-chunk:bKGD-green
            png-chunk:bKGD-blue
            png-chunk:bKGD-palette-index
            data->png-chunk:bKGD))



(define-class <png-chunk:bKGD> (<png-chunk>)
  ;; <number>
  (colour-type
   #:init-keyword #:colour-type
   #:init-value   0
   #:getter       png-chunk:bKGD-colour-type)

  ;; <number>
  (greyscale
   #:init-keyword #:greyscale
   #:init-value   0
   #:getter       png-chunk:bKGD-grayscale)

  ;; <number>
  (red
   #:init-keyword #:red
   #:init-value   0
   #:getter       png-chunk:bKGD-red)

  ;; <number>
  (green
   #:init-keyword #:green
   #:init-value   0
   #:getter       png-chunk:bKGD-green)

  ;; <number>
  (blue
   #:init-keyword #:blue
   #:init-value   0
   #:getter       png-chunk:bKGD-blue)

  ;; <number>
  (palette-index
   #:init-keyword #:palette-index
   #:init-value   0
   #:getter       png-chunk:bKGD-palette-index))



(define-method (%display (chunk <png-chunk:bKGD>) (port <port>))
  (let ((type (vector->chunk-type (png-chunk-type chunk))))
    (format port "#<png-chunk:bKGD colour type: ~a ~a>"
            (png-chunk:bKGD-colour-type chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:bKGD>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:bKGD>) (port <port>))
  (%display chunk port))



(define-method (data->png-chunk:bKGD (data        <vector>)
                                     (type        <vector>)
                                     (length      <number>)
                                     (crc         <vector>)
                                     (colour-type <number>))
  (case colour-type
    ((0 4)
     (make <png-chunk:bKGD>
       #:length             length
       #:type               type
       #:data               data
       #:crc                crc
       #:colour-type        colour-type
       #:greyscale          (vector->int16 data)))
    ((2 6)
     (make <png-chunk:bKGD>
       #:length             length
       #:type               type
       #:data               data
       #:crc                crc
       #:colour-type        colour-type
       #:red                (vector->int16 (vector-copy data 0 2))
       #:green              (vector->int16 (vector-copy data 2 4))
       #:blue               (vector->int16 (vector-copy data 4 6))))
    ((3)
     (make <png-chunk:bKGD>
       #:length             length
       #:type               type
       #:data               data
       #:crc                crc
       #:colour-type        colour-type
       #:palette-index      (vector-ref data 0)))))


;;; chunk-bkgd.scm ends here.
