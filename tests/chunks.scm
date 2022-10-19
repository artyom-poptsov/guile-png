(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (png core chunk))


(define %test-name "chunks")


(test-begin %test-name)

(test-equal "vector->chunk-type"
  '(IHDR #(73 72 68 82) "Image header")
  (vector->chunk-type #(73 72 68 82)))

(test-equal "chunk-type->vector"
  #(73 72 68 82)
  (chunk-type->vector 'IHDR))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
