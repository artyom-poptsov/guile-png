#!/usr/bin/guile \
-L modules -e main -s
!#

(use-modules (png)
             (png image)
             (png image-processing))

(define (main args)
  (let* ((png-image (png->scm))
         (threshold 100)
         (new-image (png-image-filter-solarize png-image threshold)))
    (scm->png new-image)))
