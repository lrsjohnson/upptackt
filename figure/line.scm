;;; Segment, Line, Ray Types
(define-record-type <segment>
  (segment p1 p2)
  segment?
  (p1 segment-p1)
  (p2 segment-p2))

(define-record-type <line>
  (line p1 p2)
  line?
  (p1 line-p1)
  (p2 line-p2))

(define-record-type <ray>
  (ray p1 p2)
  ray?
  (p1 ray-p1)
  (p2 ray-p2))

(define (linear-element? x)
  (or (line? x)
      (segment? x)
      (ray? x)))

;;; Alternate, helper constructors
(define (line-from-point-vec p vec)
  (let ((p2 (add-to-point p vec)))
    (line p p2)))

(define (ray-from-point-vec p vec)
  (let ((p2 (add-to-point p vec)))
    (ray p p2)))

;;; Constructors from angles

(define (ray-from-arm-1 a)
  (let ((v (angle-vertex a))
        (vec (angle-arm-1 a)))
    (ray-from-point-vec v vec)))

(define (ray-from-arm-2 a)
  (ray-from-arm-1 (reverse-angle a)))

(define (line-from-arm-1 a)
  (ray->line (ray-from-arm-1 a)))

(define (line-from-arm-2 a)
  (ray->line (ray-from-arm-1 (reverse-angle a))))

;;; Transforms

(define flip (make-generic-operation 1 'flip))

(define (flip-line l)
  (line (line-p2 l) (line-p1 l)))
(defhandler flip flip-line line?)

(define (flip-segment s)
  (segment (segment-p2 s) (segment-p1 s)))
(defhandler flip flip-segment segment?)

;;; Predicates

(define (parallel? a b)
  (vec-direction-equal? (->vec a)
                        (->vec b)))

(define (perpendicular? a b)
  (vec-perpendicular? (->vec a)
                      (->vec b)))

;;; Conversions
;;; Ray shares point p1
(define (segment->ray segment)
  (ray (segment-p1 segment)
       (segment-p2 segment)))

(define (segment->line segment)
  (line (segment-p1 segment)
        (segment-p2 segment)))

(define (ray->line ray)
  (line (ray-p1 ray)
        (ray-p2 ray)))

(define (line->vec l)
  (sub-points (line-p2 l)
              (line-p1 l)))

(define (ray->vec r)
  (sub-points (ray-p2 r)
              (ray-p1 r)))

(define (segment->vec s)
  (sub-points (segment-p2 s)
              (segment-p1 s)))

(define ->vec (make-generic-operation 1 '->vec))
(defhandler ->vec line->vec line?)
(defhandler ->vec ray->vec ray?)
(defhandler ->vec segment->vec segment?)
