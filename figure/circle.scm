;;; Circle structure
(define-record-type <circle>
  (circle center radius)
  circle?
  (center circle-center)
  (radius circle-radius))
