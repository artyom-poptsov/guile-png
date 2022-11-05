(define-module (png core filter)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png core common)
  #:export (<png-filter>
            <png-filter:none>
            <png-filter:sub>
            <png-filter:paeth>
            <png-filter:up>

            png-filter-remove!

            paeth-predictor))

(define-class <png-filter> ()
  ;; <number>
  (image-width
   #:init-keyword #:image-width
   #:getter       png-filter-image-width)

  ;; <number>
  (scanline-length
   #:init-keyword #:scanline-length
   #:getter       png-filter-scanline-length)

  (bytes-per-pixel
   #:init-keyword #:bytes-per-pixel
   #:getter       png-filter-bytes-per-pixel))



(define-method (%display (filter <png-filter>) (port <port>))
  (format port
          "#<png-filter image-width: ~apx scanline-length: ~ab bytes-per-pixel: ~ab ~a>"
          (png-filter-image-width filter)
          (png-filter-scanline-length filter)
          (png-filter-bytes-per-pixel filter)
          (object-address/hex-string filter)))

(define-method (display (filter <png-filter>) (port <port>))
  (%display filter port))

(define-method (write (filter <png-filter>) (port <port>))
  (%display filter port))



(define-class <png-filter:none> (<png-filter>))

(define-method (png-filter-remove! (filter         <png-filter:none>)
                                   (input          <bytevector>)
                                   (output         <bytevector>)
                                   (scanline-index <number>))
  "Remove the 'None' filter (RFC 2083, 6.2) from a scanline with the specified
SCANLINE-INDEX."
  (let* ((scanline-length (png-filter-scanline-length filter))
         (input-scanline-begin  (+ (* scanline-index (+ scanline-length 1)) 1))
         (output-scanline-begin (* scanline-index scanline-length)))
    (bytevector-copy! input
                      input-scanline-begin
                      output
                      output-scanline-begin
                      scanline-length)))


(define-class <png-filter:sub> (<png-filter>))

(define-method (png-filter-remove! (filter         <png-filter:sub>)
                                   (input          <bytevector>)
                                   (output         <bytevector>)
                                   (scanline-index <number>))
  "Remove the 'Sub' filter (RFC 2083, 6.3) from a scanline with the specified
SCANLINE-INDEX."
  (let* ((scanline-length       (png-filter-scanline-length filter))
         (bytes-per-pixel       (png-filter-bytes-per-pixel filter))
         (image-width           (png-filter-image-width filter))
         (input-scanline-begin  (+ (* scanline-index (+ scanline-length 1)) 1))
         (output-scanline-begin (* scanline-index scanline-length)))

    ;; Copy the first pixel as is.
    (bytevector-copy! input
                      input-scanline-begin
                      output
                      output-scanline-begin
                      bytes-per-pixel)

    (let loop ((px-index 1))
      (unless (= px-index image-width)
        (let loop-over-pixel ((index 0))
          (unless (= index bytes-per-pixel)
            (let ((absolute-input-index (+ input-scanline-begin
                                           (* px-index bytes-per-pixel)
                                           index))
                  (absolute-output-index (+ output-scanline-begin
                                            (* px-index bytes-per-pixel)
                                            index)))
              (bytevector-u8-set! output
                                  absolute-output-index
                                  (modulo (+ (bytevector-u8-ref input
                                                                absolute-input-index)
                                             (bytevector-u8-ref output
                                                                (- absolute-output-index
                                                                   bytes-per-pixel)))
                                          256))
              (loop-over-pixel (+ index 1)))))
        (loop (+ px-index 1))))))



(define-class <png-filter:paeth> (<png-filter>))

(define-method (paeth-predictor (left <number>)
                                (above <number>)
                                (upper-left <number>))
  "Paeth predictor that is implemented based on the description in RFC 2083.
The original algorithm developed by Alan W. Paeth."
  (let* ((p            (+ left (- above upper-left)))
         (p-left       (abs (- p left)))
         (p-above      (abs (- p above)))
         (p-upper-left (abs (- p upper-left))))
    (cond
     ((and (<= p-left p-above) (<= p-left p-upper-left))
      left)
     ((<= p-above p-upper-left)
      above)
     (else
      upper-left))))

(define-method (png-filter-remove! (filter         <png-filter:paeth>)
                                   (input          <bytevector>)
                                   (output         <bytevector>)
                                   (scanline-index <number>))
  (let* ((scanline-length         (png-filter-scanline-length filter))
         (image-width             (png-filter-image-width filter))
         (bytes-per-pixel         (png-filter-bytes-per-pixel filter))
         (input-scanline-begin    (+ (* scanline-index (+ scanline-length 1)) 1))
         (output-scanline-begin   (* scanline-index scanline-length))
         (previous-scanline-begin (* (- scanline-index 1) scanline-length)))
    (let loop ((px-index 0))
      (unless (= px-index image-width)
        (let loop-over-pixel ((index 0))
          (unless (= index bytes-per-pixel)
            (let* ((absolute-input-index (+ input-scanline-begin
                                            (* px-index bytes-per-pixel)
                                            index))
                   (absolute-output-index (+ output-scanline-begin
                                             (* px-index bytes-per-pixel)
                                             index))
                   (left                  (if (zero? px-index)
                                              0
                                              (bytevector-u8-ref output
                                                                 (- absolute-output-index
                                                                    bytes-per-pixel))))
                   (above                 (if (zero? scanline-index)
                                              0
                                              (bytevector-u8-ref output
                                                                 (+ previous-scanline-begin
                                                                    absolute-input-index))))
                   (upper-left            (if (zero? scanline-index)
                                              0
                                              (bytevector-u8-ref output
                                                                 (+ previous-scanline-begin
                                                                    (- absolute-input-index
                                                                       bytes-per-pixel))))))
              (bytevector-u8-set! output
                                  absolute-output-index
                                  (modulo (- (bytevector-u8-ref input
                                                                absolute-input-index)
                                             (paeth-predictor left above upper-left))
                                          256))
              (loop-over-pixel (+ index 1)))))
        (loop (+ px-index 1))))))



(define-class <png-filter:up> (<png-filter>))

(define-method (png-filter-remove! (filter         <png-filter:up>)
                                   (input          <bytevector>)
                                   (output         <bytevector>)
                                   (scanline-index <number>))
    (if (zero? scanline-index)
        (png-filter-remove! (change-class filter <png-filter>)
                            input
                            output
                            scanline-index)
        (let* ((scanline-length        (png-filter-scanline-length filter))
               (input-scanline-begin    (+ (* scanline-index (+ scanline-length 1)) 1))
               (output-scanline-begin   (* scanline-index scanline-length)))
          (let loop ((index 0))
            (unless (= index scanline-length)
              (let* ((absolute-index (+ input-scanline-begin index))
                     (raw            (bytevector-u8-ref input
                                                        absolute-index))
                     (prior          (bytevector-u8-ref output
                                                        (- absolute-index
                                                           scanline-length))))
              (bytevector-u8-set! output
                                  (+ output-scanline-begin index)
                                  (modulo (- raw prior)
                                          256))))))))

;;; filter.scm ends here.
