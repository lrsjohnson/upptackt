;;; load.scm -- Load core
(load-module "propagator")
(for-each (lambda (f) (load f))
          '("stack-queue"
            "ghelper"
            "scmutils-basic"
            "multimin"))
