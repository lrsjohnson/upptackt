;;; A Direction is equivalent to a unit vector pointing in some direction.

;;; Wrapping "directions" in a data structure to ensure in [0, 2pi], and allow for better
;;; propagation ranges, wiggling in the future.
(define-record-type <direction>
  (%direction theta)
  direction?
  (theta direction-theta))

(define (make-direction theta)
  (%direction (fix-angle-0-2pi theta)))

(define (direction-equal? d1 d2)
  (close-enuf? (direction-theta d1)
               (direction-theta d2)))
