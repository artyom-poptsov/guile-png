(define-module (png chunk-converter)
  #:use-module (oop goops)
  #:use-module (png core chunk)
  #:export (png-chunk->png-chunk:ihdr
            png-chunk->typed-chunk))

(define-method (png-chunk->png-chunk:ihdr (chunk <png-chunk>))
  (let ((data (png-chunk-data chunk)))
    (make <png-chunk:ihdr>
      #:length             (png-chunk-length chunk)
      #:type               (png-chunk-type chunk)
      #:data               (png-chunk-data chunk)
      #:crc                (png-chunk-crc chunk)
      #:width              (data:width data)
      #:height             (data:heigth data)
      #:bit-depth          (data:bit-depth data)
      #:colour-type        (data:colour-type data)
      #:compression-method (data:compression-method data)
      #:filter-method      (data:filter-method data)
      #:interlace-method   (data:interlace-method data))))


(define %converters-to-typed
  `((IHDR                  . ,png-chunk->png-chunk:ihdr)))

(define-method (png-chunk->typed-chunk (chunk <png-chunk>))
  (let ((type (png-chunk-type/name chunk)))
    (if type
        (let ((converter (assoc-ref %converters-to-typed type)))
          (if converter
              (converter chunk)
              chunk))
        (error "Unknown chunk type" type chunk))))
