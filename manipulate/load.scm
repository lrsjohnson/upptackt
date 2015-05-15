;;; load.scm -- Load manipulate
(for-each (lambda (f) (load f))
          '("linkages"
            "region"
            "constraints"
            "topology"
            "mechanism"
            "main"))
