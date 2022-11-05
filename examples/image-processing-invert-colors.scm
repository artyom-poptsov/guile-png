#!/usr/bin/guile \
-L modules -e main -s
!#

(use-modules (png)
             (png image)
             (png image-processing))

(define (main args)
  (let* ((png-image (png->scm (current-input-port)))
         (new-image (png-image-filter-invert-colors png-image)))
    (png-image->png new-image (current-output-port))))
