;;; Circle structure
(define-record-type <circle>
  (make-circle center radius)
  circle?
  (center circle-center)
  (radius circle-radius))

;;; Alternate Constructions
(define (circle-from-points center radius-point)
  (make-circle center
          (distance center radius-point)))
