(define-module (png chunk-converter)
  #:use-module (oop goops)
  #:use-module (png core chunk)
  #:use-module (png core chunk-ihdr)
  #:use-module (png core chunk-plte)
  #:use-module (png core chunk-iend)
  #:use-module (png core chunk-chrm)
  #:use-module (png core chunk-ztxt)
  #:export (png-chunk->png-chunk:IHDR
            png-chunk->png-chunk:PLTE
            png-chunk->png-chunk:IEND
            png-chunk->png-chunk:cHRM
            png-chunk->png-chunk:zTXt
            png-chunk->typed-chunk))

(define-method (png-chunk->png-chunk:IHDR (chunk <png-chunk>))
  (let ((data (png-chunk-data chunk)))
    (make <png-chunk:IHDR>
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

(define-method (png-chunk->png-chunk:PLTE (chunk <png-chunk>))
  (let ((data (png-chunk-data chunk)))
    (unless (zero? (remainder (vector-length data) 3))
      (error "Invalid PLTE chunk: data length not divisible by 3" chunk))
    (make <png-chunk:PLTE>
      #:length             (png-chunk-length chunk)
      #:type               (png-chunk-type chunk)
      #:data               (png-chunk-data chunk)
      #:crc                (png-chunk-crc chunk)
      #:palette-entries    (vector->PLTE-palette-entries data))))

(define-method (png-chunk->png-chunk:IEND (chunk <png-chunk>))
  (make <png-chunk:IEND>
    #:length             (png-chunk-length chunk)
    #:type               (png-chunk-type chunk)))

(define-method (png-chunk->png-chunk:cHRM (chunk <png-chunk>))
  (let ((data (png-chunk-data chunk)))
    (make <png-chunk:cHRM>
      #:length             (png-chunk-length chunk)
      #:type               (png-chunk-type chunk)
      #:data               (png-chunk-data chunk)
      #:crc                (png-chunk-crc chunk)
      #:white-point-x      (data:white-point-x data)
      #:white-point-y      (data:white-point-y data)
      #:red-x              (data:red-x data)
      #:red-y              (data:red-y data)
      #:green-x            (data:green-x data)
      #:green-y            (data:green-y data)
      #:blue-x             (data:blue-x data)
      #:blue-y             (data:blue-y data))))

(define-method (png-chunk->png-chunk:zTXt (chunk <png-chunk>))
  (data->png-chunk:zTXt (png-chunk-data   chunk)
                        (png-chunk-type   chunk)
                        (png-chunk-length chunk)
                        (png-chunk-crc    chunk)))


(define %converters-to-typed
  `((IHDR                  . ,png-chunk->png-chunk:IHDR)
    (PLTE                  . ,png-chunk->png-chunk:PLTE)
    (IEND                  . ,png-chunk->png-chunk:IEND)
    (cHRM                  . ,png-chunk->png-chunk:cHRM)
    (zTXt                  . ,png-chunk->png-chunk:zTXt)))

(define-method (png-chunk->typed-chunk (chunk <png-chunk>))
  (let ((type (png-chunk-type/name chunk)))
    (if type
        (let ((converter (assoc-ref %converters-to-typed type)))
          (if converter
              (converter chunk)
              chunk))
        (error "Unknown chunk type" type chunk))))
