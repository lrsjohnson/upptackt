;;; vec.scm --- Low-level vector structures

;;; Commentary:

;; Ideas:
;; - Simplifies lots of computation, cartesian coordiates
;; - Currently 2D, could extend

;; Future:
;; - Could generalize to allow for polar vs. cartesian vectors

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;; Vector Structure ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <vec>
  (make-vec dx dy)
  vec?
  (dx vec-x)
  (dy vec-y))

;;; Transformations of Vectors
(define (vec-magnitude v)
  (let ((dx (vec-x v))
        (dy (vec-y v)))
    (sqrt (+ (square dx) (square dy)))))

;;;;;;;;;;;;;;;;;;;;;;; Alternate Constructors ;;;;;;;;;;;;;;;;;;;;;;;

(define (unit-vec-from-direction direction)
  (let ((theta (direction-theta direction)))
   (make-vec (cos theta) (sin theta))))

(define (vec-from-direction-distance direction distance)
  (scale-vec (unit-vec-from-direction direction) distance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Conversions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (vec->direction v)
  (let ((dx (vec-x v))
        (dy (vec-y v)))
    (make-direction (atan dy dx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns new vecs

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

(define (scale-vec-to-dist v dist)
  (scale-vec (unit-vec v) dist))

(define (reverse-vec v)
  (make-vec (- (vec-x v))
            (- (vec-y v))))

(define (rotate-vec-90 v)
  (let ((dx (vec-x v))
        (dy (vec-y v)))
    (make-vec (- dy) dx)))

(define (unit-vec v)
  (scale-vec v (/ (vec-magnitude v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (vec-equal? v1 v2)
  (and (close-enuf? (vec-x v1)  (vec-x v2))
       (close-enuf? (vec-y v1)  (vec-y v2))))

(define (vec-direction-equal? v1 v2)
  (direction-equal?
   (vec->direction v1)
   (vec->direction v2)))

(define (vec-perpendicular? v1 v2)
  (close-enuf?
   (* (vec-x v1) (vec-x v2))
   (* (vec-y v1) (vec-y (reverse-vec v2)))))
