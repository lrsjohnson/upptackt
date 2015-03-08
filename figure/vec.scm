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

;; Rotate vector counter-clockwise
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

(define (rotate-vec-90 v)
  (let ((dx (vec-x v))
        (dy (vec-y v)))
    (make-vec (- dy) dx)))

(define (unit-vec v)
  (scale-vec v (/ (vec-magnitude v))))

(define (scale-vec-to-dist v dist)
  (scale-vec (unit-vec v) dist))
