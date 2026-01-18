(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (rnrs bytevectors)
             (ice-9 iconv)
             (oop goops)
             (png core common)
             (png core chunk)
             (png core chunk ihdr)
             (png core chunk plte)
             (png core chunk chrm)
             (png core chunk ztxt)
             (png core chunk phys)
             (png core chunk text)
             (png core chunk time)
             (png core chunk ornt)
             (png core chunk vpag))


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

(test-equal "png-chunk-decode-IHDR: width"
  3508
  (png-chunk:IHDR-width
   (png-chunk-decode-IHDR (make <png-chunk>
                            #:data   %ihdr-data
                            #:type   'IHDR
                            #:length 13
                            #:crc    (vector->int32 #vu8(119 50 219 167))))))

(test-equal "png-chunk-decode-IHDR: height"
  2480
  (png-chunk:IHDR-height
   (png-chunk-decode-IHDR (make <png-chunk>
                            #:data   %ihdr-data
                            #:type   'IHDR
                            #:length 13
                            #:crc    (vector->int32 #vu8(119 50 219 167))))))

(test-equal "png-chunk-decode-IHDR: bit-depth"
  8
  (png-chunk:IHDR-bit-depth
   (png-chunk-decode-IHDR (make <png-chunk>
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
  (let ((chunk (png-chunk-decode-IHDR (make <png-chunk:IHDR>
                                        #:data   %ihdr-data
                                        #:length 13))))
    (png-chunk-crc-update! chunk)
    (png-chunk-data (png-chunk-encode chunk))))

(define %plte-data #vu8(255 0 0 0 255 0 0 0 255))
(test-equal "png-chunk-encode: PLTE"
  %plte-data
  (let ((chunk (png-chunk-decode-PLTE (make <png-chunk>
                                        #:data   %plte-data
                                        #:length (bytevector-length %plte-data)))))
    (png-chunk-crc-update! chunk)
    (png-chunk-data (png-chunk-encode chunk))))

(define %cHRM-data
  #vu8(0 0 0 1                          ; 00..03 white-point-x
         0 0 0 2                        ; 04..07 white-point-y
         0 0 0 3                        ; 08..11 red-x
         0 0 0 4                        ; 12..15 red-y
         0 0 0 5                        ; 16..19 green-x
         0 0 0 6                        ; 20..23 green-y
         0 0 0 7                        ; 24..27 blue-x
         0 0 0 8))                      ; 28..31 blue-y
(test-equal "png-chunk-encode: cHRM"
  %cHRM-data
  (let ((chunk (png-chunk-decode-cHRM (make <png-chunk>
                                        #:data   %cHRM-data
                                        #:length (bytevector-length %cHRM-data)))))
    (png-chunk-crc-update! chunk)
    (png-chunk-data (png-chunk-encode chunk))))

(define %zTXt-data
  #vu8(102 111 111                          ; "foo"
           0                                ; NUL-separator
           0                                ; compression method
           120 156 75 74 44 2 0 2 93 1 54)) ; "bar"
(test-equal "png-chunk-encode: zTXt"
  %zTXt-data
  (let ((chunk (png-chunk-decode-zTXt (make <png-chunk>
                                        #:data %zTXt-data
                                        #:length (bytevector-length %zTXt-data)))))
    (png-chunk-crc-update! chunk)
    (png-chunk-data (png-chunk-encode chunk))))

(define %pHYs-data
  #vu8(0 0 0 8                          ; Pixels per unit, X axis
         0 0 0 16                       ; Pixels per unit, Y axis
         1))                            ; Unit specifier (meter)
(test-equal "png-chunk-encode: pHYs"
  %pHYs-data
  (let ((chunk (png-chunk-decode-pHYs (make <png-chunk>
                                        #:data %pHYs-data
                                        #:length (bytevector-length %pHYs-data)))))
    (png-chunk-crc-update! chunk)
    (png-chunk-data (png-chunk-encode chunk))))

(define %tEXt-data
  #vu8(102 111 111                      ; "foo"
         0                              ; NUL-separator
         98 97 114))                    ; "bar"
(test-equal "png-chunk-encode: tEXt"
  %tEXt-data
  (let ((chunk (png-chunk-decode-tEXt (make <png-chunk>
                                        #:data   %tEXt-data
                                        #:length (bytevector-length %tEXt-data)))))
    (png-chunk-crc-update! chunk)
    (png-chunk-data (png-chunk-encode chunk))))

(define %tIME-data
  #vu8(7 230                            ; year (2022)
         12                             ; month (12)
         18                             ; day (18)
         19                             ; hour (19)
         42                             ; minute (42)
         55))                           ; second (55)
(test-equal "png-chunk-encode: tIME"
  %tIME-data
  (let ((chunk (png-chunk-decode-tIME (make <png-chunk>
                                        #:data   %tIME-data
                                        #:length (bytevector-length %tIME-data)))))
    (png-chunk-crc-update! chunk)
    (png-chunk-data (png-chunk-encode chunk))))


;; PLTE

(test-equal "png-chunk-decode-PLTE"
  #(#vu8(255 0 0) #vu8(0 255 0) #vu8(0 0 255))
  (let* ((data #vu8(255 0 0 0 255 0 0 0 255))
         (chunk (make <png-chunk>
                 #:type 'PLTE
                 #:data data
                 #:length (bytevector-length data))))
    (png-chunk-crc-update! chunk)
    (let ((plte (png-chunk-decode-PLTE chunk)))
      (png-chunk:PLTE-palette-entries plte))))


;; ImageMagick private chunks.
(test-assert "png-chunk-encode: orNT"
  (let ((chunk (make <png-chunk:orNT>
                        #:length 1
                        #:type   'orNT
                        #:data   #vu8(1)
                        #:orientation 1)))
    (png-chunk-encode chunk)))

(test-assert "png-chunk-encode: vpAg"
  (let ((chunk (make <png-chunk:vpAg>
                 #:length 1
                 #:type   'vpAg
                 #:width 10
                 #:height 20
                 #:unit-specifier 0)))
    (png-chunk-encode chunk)))


;; Chunk cloning.

(test-assert "png-chunk-clone: pHYs"
  (let* ((chunk (make <png-chunk:pHYs>
                  #:length 9
                  #:type   'pHYs
                  #:data   #vu8(0 0 0 0 0 0 0 0 0)
                  #:crc    0
                  #:pixels-per-unit-x-axis 11811
                  #:pixels-per-unit-y-axis 11811
                  #:unit-specifier         1))
         (chunk-clone (png-chunk-clone chunk)))
    (and (equal? (png-chunk-length chunk) (png-chunk-length chunk-clone))
         (equal? (png-chunk-crc chunk) (png-chunk-crc chunk-clone))
         (equal? (png-chunk-data chunk) (png-chunk-data chunk-clone))
         (not (eq? (png-chunk-data chunk) (png-chunk-data chunk-clone)))
         (equal? (png-chunk:pHYs-pixels-per-unit-x-axis chunk)
                 (png-chunk:pHYs-pixels-per-unit-x-axis chunk-clone))
         (equal? (png-chunk:pHYs-pixels-per-unit-y-axis chunk)
                 (png-chunk:pHYs-pixels-per-unit-y-axis chunk-clone)))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
