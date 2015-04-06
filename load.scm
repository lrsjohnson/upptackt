;;; Start from scratch
(set! *random-state* (fasload "a-random-state"))

(for-each (lambda (f) (load f))
          '("lib/ghelper"
            "lib/eq-properties"
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
            "graphics"
            "investigations"
            "main"))
