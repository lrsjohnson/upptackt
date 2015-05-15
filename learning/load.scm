;;; load.scm -- Load learning module
(for-each (lambda (f) (load f))
          '("definitions"
            "student"
            "core-knowledge"))
