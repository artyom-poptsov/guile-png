#!/usr/bin/guile \
-L modules -e main -s
!#

(use-modules (png)
             (png image)
             (png image-processing))

(define (main args)
  (let* ((png-image (png->scm (current-input-port)))
         (threshold 100)
         (new-image (png-image-filter-solarize png-image threshold)))
    (png-image->png new-image (current-output-port))))
