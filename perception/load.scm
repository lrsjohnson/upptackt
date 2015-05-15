;;; load.scm -- Load perception
(for-each (lambda (f) (load f))
          '("relationship"
            "observation"
            "analyzer"))
