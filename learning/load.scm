;;; load.scm -- Load learning module
(for-each (lambda (f) (load f))
          '("definitions"
            "student"
            "conjecture"
            "simplifier"
            "core-knowledge"))
