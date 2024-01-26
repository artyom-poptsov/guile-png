(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (rnrs bytevectors)
             (ice-9 iconv)
             (oop goops)
             (png core common)
             (png core filter))


(define %test-name "core-filter")


(test-begin %test-name)

(test-equal "png-filter-algorithm-type->name: 0 -> NONE"
  'NONE
  (png-filter-algorithm-type->name 0))

(test-equal "png-filter-algorithm-name->type: NONE -> 0"
  0
  (png-filter-algorithm-name->type 'NONE))

(test-assert "<png-filter>: display"
  (with-output-to-string
    (lambda ()
      (display (make <png-filter>
                 #:scanline-length 0
                 #:bytes-per-pixel 0)))))

(test-equal "png-filter-type"
  0
  (png-filter-type (make <png-filter:none>)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
