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
          '("lib/stack-queue"
            "utils"
            "lib/ghelper"
            "lib/scmutils-basic"
            "lib/multimin"
            "macros"
            "analyzer"
            "animation"
            "figure/line"
            "figure/direction"
            "figure/direction-interval"
            "figure/vec"
            "figure/measurements"
            "figure/angle"
            "figure/bounds"
            "figure/circle"
            "figure/constructions"
            "figure/intersections"
            "figure/figure"
            "figure/math-utils"
            "figure/point"
            "figure/polygon"
            "figure/metadata"
            "figure/dependencies"
            "figure/randomness"
            "figure/transforms"
            "appearance"
            "graphics"
            "investigations"
            "manipulate/linkages"
            "manipulate/region"
            "manipulate/constraints"
            "manipulate/topology"
            "manipulate/mechanism"
            "main"))
(initialize-scheduler)

'done-loading
