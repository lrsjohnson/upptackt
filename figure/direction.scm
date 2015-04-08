;;; A Direction is equivalent to a unit vector pointing in some direction.

;;; Wrapping "directions" in a data structure to ensure in [0, 2pi], and allow for better
;;; propagation ranges, wiggling in the future.
(define-record-type <direction>
  (%direction theta)
  direction?
  (theta direction-theta))

(define (make-direction theta)
  (%direction (fix-angle-0-2pi theta)))

;;; Vec from direction
;;; TODO: Maybe allow directions to be represented as dx, dy
(define (vec-from-direction direction)
  (unit-vec-from-direction direction))

;;; Arithmetic
(define (add-to-direction dir radians)
  (make-direction (+ (direction-theta dir)
                     radians)))
;;; D2 - D1
(define (subtract-directions d2 d1)
  (fix-angle-0-2pi (- (direction-theta d2)
                      (direction-theta d1))))

(define (rotate-direction-90 dir)
  (add-to-direction dir (/ pi 2)))

(define (reverse-direction dir)
  (add-to-direction dir pi))

;;; Predicates on Direction
(define (direction-equal? d1 d2)
  (close-enuf? (direction-theta d1)
               (direction-theta d2)))

(define (direction-opposite? d1 d2)
  (close-enuf? (direction-theta d1)
               (direction-theta (reverse-direction d2))))

(define (direction-perpendicular? d1 d2)
  (close-enuf?
   (abs (- (direction-theta d1)
           (direction-theta d2)))
   (/ pi 2)))


(define (direction-parallel? d1 d2)
  (or (direction-equal? d1 d2)
      (direction-opposite? d1 d2)))
