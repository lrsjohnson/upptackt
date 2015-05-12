;;; circle.scm --- Circles

;;; Commentary:

;; Ideas:
;; - Currently rather limited support for circles

;; Future:
;; - Arcs, tangents, etc.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;; Circle structure ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <circle>
  (make-circle center radius)
  circle?
  (center circle-center)
  (radius circle-radius))

;;;;;;;;;;;;;;;;;;;;;; Alternate Constructions ;;;;;;;;;;;;;;;;;;;;;;;

(define (circle-from-points center radius-point)
  (make-circle center
          (distance center radius-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Points on circle ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (point-on-circle-in-direction cir dir)
  (let ((center (circle-center cir))
        (radius (circle-radius cir)))
    (add-to-point
     center
     (vec-from-direction-distance
      dir radius))))
