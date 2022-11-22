(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (png core common)
             (png core chunk)
             (png core chunk ihdr))


(define %test-name "chunks")


(test-begin %test-name)

(test-equal "png-chunk?: #t"
  #t
  (png-chunk? (make <png-chunk> #:type 'IHDR)))

(test-equal "png-chunk?: #f"
  #f
  (png-chunk? "not-a-chunk"))

(test-equal "vector->chunk-type"
  '(IHDR #vu8(73 72 68 82) "Image header")
  (vector->chunk-type #vu8(73 72 68 82)))

(test-equal "chunk-type->vector"
  #vu8(73 72 68 82)
  (chunk-type->vector 'IHDR))

(test-equal "png-chunk-type/description"
  "Image header"
  (png-chunk-type/description (make <png-chunk>
                                #:type 'IHDR)))


;; Chunk naming.

(test-equal "png-chunk-safe-to-copy?: #t"
  #t
  (png-chunk-safe-to-copy? #vu8(73 72 68 82)))

(test-equal "png-chunk-safe-to-copy?: #f"
  #f
  (png-chunk-safe-to-copy? #vu8(122 84 88 116)))


;; IHDR

(define %ihdr-data #vu8(0 0 13 180 0 0 9 176 8 2 0 0 0))

(test-equal "data->png-chunk:IHDR: width"
  3508
  (png-chunk:IHDR-width
   (data->png-chunk:IHDR %ihdr-data
                         'IHDR
                         13
                         (vector->int32 #vu8(119 50 219 167)))))

(test-equal "data->png-chunk:IHDR: height"
  2480
  (png-chunk:IHDR-height
   (data->png-chunk:IHDR %ihdr-data
                         'IHDR
                         13
                         (vector->int32 #vu8(119 50 219 167)))))

(test-equal "data->png-chunk:IHDR: bit-depth"
  8
  (png-chunk:IHDR-bit-depth
   (data->png-chunk:IHDR %ihdr-data
                         'IHDR
                         13
                         (vector->int32 #vu8(119 50 219 167)))))


;; CRC

(test-assert "png-chunk-crc-calculate"
  (let ((chunk (data->png-chunk:IHDR %ihdr-data
                                     'IHDR
                                     13
                                     (vector->int32 #vu8(119 50 219 167)))))
    (png-chunk-crc-calculate chunk)))



(test-equal "equal?: #t"
  #t
  (let ((chunk1 (make <png-chunk>
                  #:type 'IHDR))
        (chunk2 (make <png-chunk>
                  #:type 'IHDR)))
    (png-chunk-crc-update! chunk1)
    (png-chunk-crc-update! chunk2)
    (equal? chunk1 chunk2)))

(test-equal "equal?: #f"
  #f
  (let ((chunk1 (make <png-chunk>
                  #:type 'IHDR))
        (chunk2 (make <png-chunk>
                  #:type 'IEND)))
    (png-chunk-crc-update! chunk1)
    (png-chunk-crc-update! chunk2)
    (equal? chunk1 chunk2)))

(test-equal "png-chunk-clone"
  #t
  (let ((chunk1 (make <png-chunk> #:type 'IHDR)))
    (png-chunk-crc-update! chunk1)
    (let ((chunk2 (png-chunk-clone chunk1)))
      (equal? chunk1 chunk2))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
