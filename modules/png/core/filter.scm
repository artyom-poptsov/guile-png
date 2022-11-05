(define-module (png core filter)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:export (png-filter-none-remove!
            png-filter-sub-remove!

            paeth-predictor))



(define* (png-filter-none-remove! input
                                  output
                                  #:key
                                  scanline-length
                                  scanline-index)
  "Remove the 'None' filter (RFC 2083, 6.2) from a scanline with the specified
SCANLINE-INDEX."
  (let ((input-scanline-begin  (+ (* scanline-index (+ scanline-length 1)) 1))
        (output-scanline-begin (* scanline-index scanline-length)))
    (bytevector-copy! input
                      input-scanline-begin
                      output
                      output-scanline-begin
                      scanline-length)))

(define* (png-filter-sub-remove! input
                                 output
                                 #:key
                                 image-width
                                 scanline-length
                                 scanline-index
                                 bytes-per-pixel)
  "Remove the 'Sub' filter (RFC 2083, 6.3) from a scanline with the specified
SCANLINE-INDEX."
  (let ((input-scanline-begin  (+ (* scanline-index (+ scanline-length 1)) 1))
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

;;; filter.scm ends here.
