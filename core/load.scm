;;; load.scm -- Load core
(for-each (lambda (f) (load f))
          '("utils"
            "macros"
            "print"
            "animation"))
