(define-module (png image)
  #:use-module (oop goops)
  #:use-module (png core chunk)
  #:export (<png-image>
            png-image-chunks
            png-image-chunks-query
            png-image-chunks-filter))


(define-class <png-image> ()
  ;; <list> of <png-chunk>
  (chunks
   #:init-value   '()
   #:init-keyword #:chunks
   #:getter       png-image-chunks))


(define-method (png-image-chunks-filter (image <png-image>) (predicate <procedure>))
  (filter predicate (png-image-chunks image)))

(define-method (png-image-chunks-query (image <png-image>) (chunk <symbol>))
  (png-image-chunks-filter image (lambda (c)
                                   (equal? (png-chunk-type/name c) chunk))))

(define-method (png-image-chunks-query (image <png-image>) (chunk <vector>))
  (png-image-chunks-filter image (lambda (c)
                                   (equal? (png-chunk-type c) chunk))))

;; image.scm ends here.
