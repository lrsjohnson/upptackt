;;; load.scm -- Load core
(load-module "propagator")
(for-each (lambda (f) (load f))
          '("close-enuf"
            "ghelper"
            "scmutils-basic"))
