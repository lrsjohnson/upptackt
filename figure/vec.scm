;;; Structures For computation, not display
(define-record-type <vec>
  (make-vec dx dy)
  vec?
  (dx vec-x)
  (dy vec-y))

;;; Computations of Vectors

;;; Transformations of Vectors
(define (vec-magnitude v)
  (let ((dx (vec-x v))
        (dy (vec-y v)))
    (sqrt (+ (square dx) (square dy)))))

(define (vec-to-angle v)
  (let ((dx (vec-x v))
        (dy (vec-y v)))
    (atan dy dx)))

;;; Rotate vector counter-clockwise
(define (rotate-vec v radians)
  (let ((dx (vec-x v))
        (dy (vec-y v))
        (c (cos radians))
        (s (sin radians)))
    (make-vec (+ (* c dx) (- (* s dy)))
              (+ (* s dx) (* c dy)))))

(define (scale-vec v c)
  (let ((dx (vec-x v))
        (dy (vec-y v)))
    (make-vec (* c dx) (* c dy))))

;;; Helpful creators for vector manipulation

(define (unit-vec-from-angle theta)
  (make-vec (cos theta) (sin theta)))

(define (reverse-vec v)
  (make-vec (- (vec-x v))
            (- (vec-y v))))

(define (rotate-vec-90 v)
  (let ((dx (vec-x v))
        (dy (vec-y v)))
    (make-vec (- dy) dx)))

(define (unit-vec v)
  (scale-vec v (/ (vec-magnitude v))))

(define (scale-vec-to-dist v dist)
  (scale-vec (unit-vec v) dist))

;;; Compare Equality
(define (vec-equal? v1 v2)
  (and (close-enuf? (vec-x v1)  (vec-x v2))
       (close-enuf? (vec-y v1)  (vec-y v2))))

(define (vec-direction-equal? v1 v2)
  (close-enuf?
   (vec-to-angle v1)
   (vec-to-angle v2)))

(define (vec-perpendicular? v1 v2)
  (close-enuf?
   (* (vec-x v1) (vec-x v2))
   (* (vec-y v1) (vec-y (reverse-vec v2)))))
