(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (rnrs bytevectors)
             (ice-9 iconv)
             (oop goops)
             (png core common)
             (png core chunk)
             (png core chunk ihdr)
             (png core chunk plte))


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

(test-equal "png-chunk-ancillary?: #t"
  #t
  (png-chunk-ancillary? (chunk-type->vector 'sRGB)))

(test-equal "png-chunk-ancillary?: #f"
  #f
  (png-chunk-ancillary? (chunk-type->vector 'IHDR)))

(test-equal "png-chunk-private?: #t"
  #t
  (png-chunk-private? (string->bytevector "TeST" "ASCII")))

(test-equal "png-chunk-private?: #f"
  #f
  (png-chunk-private? (chunk-type->vector 'IHDR)))

(test-equal "png-chunk-safe-to-copy?: #t"
  #t
  (png-chunk-safe-to-copy? #vu8(73 72 68 82)))

(test-equal "png-chunk-safe-to-copy?: #f"
  #f
  (png-chunk-safe-to-copy? #vu8(122 84 88 116)))


;; IHDR

(define %ihdr-data #vu8(0 0 13 180 0 0 9 176 8 2 0 0 0))

(test-equal "png-chunk->png-chunk:IHDR: width"
  3508
  (png-chunk:IHDR-width
   (png-chunk->png-chunk:IHDR (make <png-chunk>
                                #:data   %ihdr-data
                                #:type   'IHDR
                                #:length 13
                                #:crc    (vector->int32 #vu8(119 50 219 167))))))

(test-equal "png-chunk->png-chunk:IHDR: height"
  2480
  (png-chunk:IHDR-height
   (png-chunk->png-chunk:IHDR (make <png-chunk>
                                #:data   %ihdr-data
                                #:type   'IHDR
                                #:length 13
                                #:crc    (vector->int32 #vu8(119 50 219 167))))))

(test-equal "png-chunk->png-chunk:IHDR: bit-depth"
  8
  (png-chunk:IHDR-bit-depth
   (png-chunk->png-chunk:IHDR (make <png-chunk>
                                #:data   %ihdr-data
                                #:type   'IHDR
                                #:length 13
                                #:crc    (vector->int32 #vu8(119 50 219 167))))))


;; CRC

(test-assert "png-chunk-crc-calculate"
  (let ((chunk (make <png-chunk>
                 #:data   %ihdr-data
                 #:type   'IHDR
                 #:length 13
                 #:crc    (vector->int32 #vu8(119 50 219 167)))))
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

(test-equal "png-chunk-encode: IHDR"
  %ihdr-data
  (let ((chunk (png-chunk->png-chunk:IHDR (make <png-chunk:IHDR>
                                            #:data   %ihdr-data
                                            #:length 13))))
    (png-chunk-crc-update! chunk)
    (png-chunk-data chunk)))


;; PLTE

(test-equal "png-chunk->png-chunk:PLTE"
  #(#vu8(255 0 0) #vu8(0 255 0) #vu8(0 0 255))
  (let* ((data #vu8(255 0 0 0 255 0 0 0 255))
         (chunk (make <png-chunk>
                 #:type 'PLTE
                 #:data data
                 #:length (bytevector-length data))))
    (png-chunk-crc-update! chunk)
    (let ((plte (png-chunk->png-chunk:PLTE chunk)))
      (png-chunk:PLTE-palette-entries plte))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
