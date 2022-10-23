(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (png core chunk)
             (png core chunk-ihdr))


(define %test-name "chunks")


(test-begin %test-name)

(test-equal "vector->chunk-type"
  '(IHDR #vu8(73 72 68 82) "Image header")
  (vector->chunk-type #vu8(73 72 68 82)))

(test-equal "chunk-type->vector"
  #vu8(73 72 68 82)
  (chunk-type->vector 'IHDR))

(test-equal "png-chunk-type/name"
  'IHDR
  (png-chunk-type/name (make <png-chunk>
                         #:type #vu8(73 72 68 82))))

(test-equal "png-chunk-type/description"
  "Image header"
  (png-chunk-type/description (make <png-chunk>
                                #:type #vu8(73 72 68 82))))


;; IHDR

(define %ihdr-data #vu8(0 0 13 180 0 0 9 176 8 2 0 0 0))

(test-equal "data->png-chunk:IHDR: width"
  3508
  (png-chunk:IHDR-width
   (data->png-chunk:IHDR %ihdr-data
                         #vu8(73 72 68 82)
                         13
                         #vu8(119 50 219 167))))

(test-equal "data->png-chunk:IHDR: height"
  2480
  (png-chunk:IHDR-height
   (data->png-chunk:IHDR %ihdr-data
                         #vu8(73 72 68 82)
                         13
                         #vu8(119 50 219 167))))

(test-equal "data->png-chunk:IHDR: bit-depth"
  8
  (png-chunk:IHDR-bit-depth
   (data->png-chunk:IHDR %ihdr-data
                         #vu8(73 72 68 82)
                         13
                         #vu8(119 50 219 167))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
