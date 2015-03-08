;;; Start from scratch
(ge (make-top-level-environment))

(for-each (lambda (f) (load f))
          '("lib/ghelper"
            "lib/eq-properties"
            "utils"
            "macros"
            "analyzer"
            "figure/utils"
            "figure/line"
            "figure/vec"
            "figure/measurements"
            "figure/angle"
            "figure/bounds"
            "figure/circle"
            "figure/constructions"
            "figure/figure"
            "figure/math-utils"
            "figure/point"
            "figure/metadata"
            "figure/randomness"
            "figure/transforms"
            "graphics"
            "investigations"
            "main"))
