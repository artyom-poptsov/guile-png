(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (png core crc))


(define %test-name "chunks")

(test-begin %test-name)



(test-equal "crc"
  #x3488DF43
  (crc  #vu8(32 10)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
