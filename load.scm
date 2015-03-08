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
            "figure/angle"
            "figure/bounds"
            "figure/circle"
            "figure/constructions"
            "figure/figure"
            "figure/math-utils"
            "figure/measurements"
            "figure/point"
            "figure/metadata"
            "figure/randomness"
            "figure/transforms"
            "figure/vec"
            "graphics"
            "investigations"
            "main"))
