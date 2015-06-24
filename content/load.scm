;;; load.scm -- Load learning module
(for-each (lambda (f) (load f))
          '("investigations"
            "random-polygons"
            "thesis-demos"))
