;;; load.scm -- Load graphics
(for-each (lambda (f) (load f))
          '("appearance"
            "graphics"))
