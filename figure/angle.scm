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
    (assert (on-line? vertex l) "Angle-from-line-ray: Vertex of ray not on line")
    (let ((v1 (line->vec l))
          (v2 (ray->vec r)))
      (make-angle v1 vertex v2))))
(defhandler angle-from angle-from-line-ray line? ray?)

(define (angle-from-ray-line r l)
  (reverse-angle (angle-from-line-ray l r)))
(defhandler angle-from angle-from-ray-line ray? line?)

(define (angle-from-segment-segment s1 s2)
  (define (angle-from-segment-internal s1 s2)
    (let ((vertex (segment-p1 s1)))
      (let ((v1 (segment->vec s1))
            (v2 (segment->vec s2)))
        (make-angle v1 vertex v2))))
  (cond ((point-equal? (segment-p1 s1)
                       (segment-p1 s2))
         (angle-from-segment-internal s1 s2))
        ((point-equal? (segment-p2 s1)
                       (segment-p1 s2))
         (angle-from-segment-internal (flip s1) s2))
        ((point-equal? (segment-p1 s1)
                       (segment-p2 s2))
         (angle-from-segment-internal s1 (flip s2)))
        ((point-equal? (segment-p2 s1)
                       (segment-p2 s2))
         (angle-from-segment-internal  (flip s1) (flip s2)))
        (else (error "Angle-from-segment-segment must share vertex"))))
(defhandler angle-from angle-from-segment-segment segment? segment?)



(define (smallest-angle-from a b)
  (smallest-angle (angle-from a b)))

;;; Predicates on Angles
(define (angle-measure-equal? a1 a2)
  (close-enuf? (angle-measure a1)
               (angle-measure a2)))

(define (supplementary-angles? a1 a2)
  (close-enuf? (+ (angle-measure a1)
                  (angle-measure a2))
               pi))

(define (complementary-angles? a1 a2)
  (close-enuf? (+ (angle-measure a1)
                  (angle-measure a2))
               (/ pi 2.0)))
