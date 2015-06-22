;;; topology.scm --- Helpers for establishing topology for mechanism

;;; Commentary:

;; Ideas:
;; - Simplify listing out all bar and joint orderings
;; - Start with basic polygons, etc.

;; Future:
;; - Figure out making multi-in/out joints: (all pairs?)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;; Establish-topology ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CCW point names
(define (m:establish-polygon-topology . point-names)
  (if (< (length point-names) 3)
      (error "Min polygon size: 3"))
  (let ((extended-point-names
         (append point-names (list (car point-names) (cadr point-names)))))
    (let ((bars (map (lambda (p1-name p2-name)
                       (m:make-named-bar p1-name p2-name))
                     point-names
                     (cdr extended-point-names)))
          (joints (map (lambda (p1-name vertex-name p2-name)
                         (m:make-named-joint p1-name vertex-name p2-name))
                       (cddr extended-point-names)
                       (cdr extended-point-names)
                       point-names)))
      (append bars joints
              (list (m:polygon-sum-slice (map m:joint-name joints)))))))
