;;; load.scm -- Load learning module
(for-each (lambda (f) (load f))
          '("lattice"
            "definitions"
            "student"
            "conjecture"
            "simplifier"
            "core-knowledge"
            "interface"
            "investigation"))
