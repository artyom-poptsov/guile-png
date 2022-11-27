(define-module (png chunk-converter)
  #:use-module (oop goops)
  #:use-module (png core chunk)
  #:use-module (png core chunk ihdr)
  #:use-module (png core chunk plte)
  #:use-module (png core chunk iend)
  #:use-module (png core chunk chrm)
  #:use-module (png core chunk text)
  #:use-module (png core chunk ztxt)
  #:use-module (png core chunk time)
  #:use-module (png core chunk iccp)
  #:use-module (png core chunk phys)
  #:use-module (png core chunk bkgd)
  #:use-module (png image)
  #:export (png-chunk->typed-chunk))


(define (make-converter proc)
  (lambda (image chunk)
    (proc chunk)))

(define %converters-to-typed
  `((IHDR . ,(make-converter png-chunk->png-chunk:IHDR))
    (PLTE . ,(make-converter png-chunk->png-chunk:PLTE))
    (IEND . ,(make-converter png-chunk->png-chunk:IEND))
    (cHRM . ,(make-converter png-chunk->png-chunk:cHRM))
    (tEXt . ,(make-converter png-chunk->png-chunk:tEXt))
    (tEXT . ,(make-converter png-chunk->png-chunk:tEXt))
    (zTXt . ,(make-converter png-chunk->png-chunk:zTXt))
    (tIME . ,(make-converter png-chunk->png-chunk:tIME))
    (iCCP . ,(make-converter png-chunk->png-chunk:iCCP))
    (pHYs . ,(make-converter png-chunk->png-chunk:pHYs))
    (bKGD . ,(lambda (image chunk)
               (let ((result (png-image-chunks-query image 'IHDR)))
                 (unless result
                   (error "Could not find IHDR chunk"))
                 (data->png-chunk:bKGD chunk (car result)))))))



(define-method (png-chunk->typed-chunk image
                                       (chunk <png-chunk>))
  (let ((type (png-chunk-type chunk)))
    (if type
        (let ((converter (assoc-ref %converters-to-typed type)))
          (if converter
              (converter image chunk)
              chunk))
        (error "Unknown chunk type" type chunk))))
