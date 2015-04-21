;;; Start from scratch
(set! *random-state* (fasload "a-random-state"))

(define (reset)
  (ge (make-top-level-environment))
  (load "load"))

(load "lib/eq-properties")

(cd "lib/propagator")
(load "load")
(cd "../..")

(for-each (lambda (f) (load f))
          '("lib/ghelper"
            "utils"
            "macros"
            "analyzer"
            "animation"
            "figure/utils"
            "figure/line"
            "figure/direction"
            "figure/vec"
            "figure/measurements"
            "figure/angle"
            "figure/bounds"
            "figure/circle"
            "figure/constructions"
            "figure/figure"
            "figure/math-utils"
            "figure/point"
            "figure/polygon"
            "figure/metadata"
            "figure/randomness"
            "figure/transforms"
            "appearance"
            "graphics"
            "investigations"
            "manipulate/linkages"
            "main"))

(initialize-scheduler)
