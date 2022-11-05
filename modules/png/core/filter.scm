(define-module (png core filter)
  #:use-module (rnrs bytevectors)
  #:export (paeth-predictor
            png-filter-none-remove!))



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

;;; filter.scm ends here.
