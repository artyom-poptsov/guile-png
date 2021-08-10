(define-module (png chunk-converter)
  #:use-module (oop goops)
  #:use-module (png core chunk)
  #:use-module (png core chunk-ihdr)
  #:use-module (png core chunk-plte)
  #:use-module (png core chunk-iend)
  #:use-module (png core chunk-chrm)
  #:use-module (png core chunk-ztxt)
  #:use-module (png core chunk-time)
  #:use-module (png core chunk-iccp)
  #:use-module (png core chunk-phys)
  #:export (png-chunk->typed-chunk))


(define %converters-to-typed
  `((IHDR . ,data->png-chunk:IHDR)
    (PLTE . ,data->png-chunk:PLTE)
    (IEND . ,data->png-chunk:IEND)
    (cHRM . ,data->png-chunk:cHRM)
    (zTXt . ,data->png-chunk:zTXt)
    (tIME . ,data->png-chunk:tIME)
    (iCCP . ,data->png-chunk:iCCP)
    (pHYs . ,data->png-chunk:pHYs)))

(define-method (png-chunk->typed-chunk (chunk <png-chunk>))
  (let ((type (png-chunk-type/name chunk)))
    (if type
        (let ((converter (assoc-ref %converters-to-typed type)))
          (if converter
              (converter (png-chunk-data   chunk)
                         (png-chunk-type   chunk)
                         (png-chunk-length chunk)
                         (png-chunk-crc    chunk))
              chunk))
        (error "Unknown chunk type" type chunk))))
