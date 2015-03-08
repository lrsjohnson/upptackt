;;; v1 and v2 are vectors in the directions of the angle arms
(define-record-type <angle>
  (make-angle arm1 vertex arm2)
  angle?
  (arm1 angle-arm-1)
  (vertex angle-vertex)
  (arm2 angle-arm-2))

;;; Transformations on Angles

(define (reverse-angle a)
  (let ((a1 (angle-arm-1 a))
        (v (angle-vertex a))
        (a2 (angle-arm-2 a)))
    (make-angle a2 v a1)))

(define (smallest-angle a)
  (if (> (angle-measure a) pi)
      (reverse-angle a)
      a))

;;; Alternate Constructors

(define (angle-from-points p1 vertex p2)
  (let ((v1 (sub-points p1 vertex))
        (v2 (sub-points p2 vertex)))
    (make-angle v1 vertex v2)))

;;; Angle from pairs of lines / rays / segments

(define angle-from (make-generic-operation 2 'angle-from))

(define (angle-from-lines l1 l2)
  (let ((v1 (line->vec l1))
        (v2 (line->vec l2))
        (p (intersect-lines l1 l2)))
    (make-angle v1 p v2)))
(defhandler angle-from angle-from-lines line? line?)

(define (angle-from-line-ray l r)
  (let ((vertex (ray-p1 r)))
    ;(assert (on-line? vertex l) "Angle-from-line-ray: Vertex of ray not on line")
    (let ((v1 (line->vec l))
          (v2 (line->vec r)))
      (make-angle v1 vertex v2))))
(defhandler angle-from angle-from-line-ray line? ray?)

(define (angle-from-ray-line r l)
  (reverse-angle (angle-from-line-ray l r)))
(defhandler angle-from angle-from-ray-line ray? line?)

(define (smallest-angle-from a b)
  (smallest-angle (angle-from a b)))
