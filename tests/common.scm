(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (png core common))

(define %test-name "common")


(test-begin %test-name)

(test-equal "int32->bytevector: One byte."
  #vu8(0 0 0 255)
  (int32->bytevector 255))

(test-equal "int32->bytevector: Two bytes."
  #vu8(0 0 1 0)
  (int32->bytevector 256))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
