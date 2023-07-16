(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (png core filter))


(define %test-name "filter")

(test-begin %test-name)



(test-equal "png-filter-algorithm-type->name"
  'NONE
  (png-filter-algorithm-type->name 0))

(test-equal "png-filter-algorithm-name->type"
  0
  (png-filter-algorithm-name->type 'NONE))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
