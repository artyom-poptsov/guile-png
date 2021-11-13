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
  #:use-module (png core chunk-bkgd)
  #:use-module (png image)
  #:export (png-chunk->typed-chunk))


(define (make-converter proc)
  (lambda (image chunk)
    (proc (png-chunk-data   chunk)
          (png-chunk-type   chunk)
          (png-chunk-length chunk)
          (png-chunk-crc    chunk))))

(define %converters-to-typed
  `((IHDR . ,(make-converter data->png-chunk:IHDR))
    (PLTE . ,(make-converter data->png-chunk:PLTE))
    (IEND . ,(make-converter data->png-chunk:IEND))
    (cHRM . ,(make-converter data->png-chunk:cHRM))
    (zTXt . ,(make-converter data->png-chunk:zTXt))
    (tIME . ,(make-converter data->png-chunk:tIME))
    (iCCP . ,(make-converter data->png-chunk:iCCP))
    (pHYs . ,(make-converter data->png-chunk:pHYs))
    (bKGD . ,(lambda (image chunk)
               (data->png-chunk:bKGD (png-chunk-data   chunk)
                                     (png-chunk-type   chunk)
                                     (png-chunk-length chunk)
                                     (png-chunk-crc    chunk)
                                     (png-chunk:IHDR-colour-type
                                      (car (png-image-chunks-query image 'IHDR))))))))



(define-method (png-chunk->typed-chunk (image <png-image>)
                                       (chunk <png-chunk>))
  (let ((type (png-chunk-type/name chunk)))
    (if type
        (let ((converter (assoc-ref %converters-to-typed type)))
          (if converter
              (converter image chunk)
              chunk))
        (error "Unknown chunk type" type chunk))))
