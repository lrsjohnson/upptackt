;;; Circle structure
(define-record-type <circle>
  (circle center radius)
  circle?
  (center circle-center)
  (radius circle-radius))

;;; Alternate Constructions
(define (circle-from-points center radius-point)
  (circle center
          (distance center radius-point)))
