;;; load.scm -- Load the system

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reset)
  (ignore-errors (lambda () (close)))
  (ge (make-top-level-environment))
  (load "load"))

(define (load-module subdirectory)
  (let ((cur-pwd (pwd)))
    (cd subdirectory)
    (load "load")
    (cd cur-pwd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Load Modules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(for-each (lambda (m) (load-module m))
          '("lib"
            "core"
            "figure"
            "graphics"
            "solver"
            "perception"
            "learning"
            "content"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Initialize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define c (if (environment-bound? (the-environment) 'c) c (canvas)))

(define (close) (ignore-errors (lambda () (graphics-close (canvas-g c)))))


(set! *random-state* (fasload "a-random-state"))
(initialize-scheduler)
(initialize-student)

'done-loading
