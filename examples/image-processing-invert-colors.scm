#!/usr/bin/guile \
-L modules -e main -s
!#

(use-modules (png)
             (png image)
             (png image-processing))

(define (main args)
  (let* ((png-image (png->scm))
         (new-image (png-image-filter-invert-colors png-image)))
    (scm->png new-image)))
