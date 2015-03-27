;;; v1 and v2 are vectors in the directions of the angle arms
(define-record-type <angle>
  (make-angle dir1 vertex dir2)
  angle?
  (dir1 angle-arm-1)
  (vertex angle-vertex)
  (dir2 angle-arm-2))

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
  (let ((arm1 (direction-from-points p1 vertex))
        (arm2 (direction-from-points p2 vertex)))
    (make-angle arm1 vertex arm2)))

;;; Angle from pairs of lines / rays / segments

(define angle-from (make-generic-operation 2 'angle-from))

(define (angle-from-lines l1 l2)
  (let ((v1 (line->direction l1))
        (v2 (line->direction l2))
        (p (intersect-lines l1 l2)))
    (make-angle d1 p d2)))
(defhandler angle-from angle-from-lines line? line?)

(define (angle-from-line-ray l r)
  (let ((vertex (ray-p1 r)))
    (assert (on-line? vertex l) "Angle-from-line-ray: Vertex of ray not on line")
    (let ((d1 (line->direction l))
          (d2 (ray->direction r)))
      (make-angle d1 vertex d2))))
(defhandler angle-from angle-from-line-ray line? ray?)

(define (angle-from-ray-line r l)
  (reverse-angle (angle-from-line-ray l r)))
(defhandler angle-from angle-from-ray-line ray? line?)

(define (angle-from-segment-segment s1 s2)
  (define (angle-from-segment-internal s1 s2)
    (let ((vertex (segment-endpoint-1 s1)))
      (let ((d1 (segment->direction s1))
            (d2 (segment->direction s2)))
        (make-angle d1 vertex d2))))
  (cond ((point-equal? (segment-endpoint-1 s1)
                       (segment-endpoint-1 s2))
         (angle-from-segment-internal s1 s2))
        ((point-equal? (segment-endpoint-2 s1)
                       (segment-endpoint-1 s2))
         (angle-from-segment-internal (flip s1) s2))
        ((point-equal? (segment-endpoint-1 s1)
                       (segment-endpoint-2 s2))
         (angle-from-segment-internal s1 (flip s2)))
        ((point-equal? (segment-endpoint-2 s1)
                       (segment-endpoint-2 s2))
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

;;; Definitions
;;; TODO? Consider learning or putiting elsewhere
(define (linear-pair? a1 a2)
  (define (linear-pair-internal? a1 a2)
    (and (point-equal? (angle-vertex a1)
                       (angle-vertex a2))
         (direction-equal? (angle-arm-2 a1)
                           (angle-arm-1 a2))
         (direction-opposite? (angle-arm-1 a1)
                               (angle-arm-2 a2))))
  (or (linear-pair-internal? a1 a2)
      (linear-pair-internal? a2 a1)))

(define (vertical-angles? a1 a2)
  (and (point-equal? (angle-vertex a1)
                     (angle-vertex a2))
       (direction-opposite? (angle-arm-1 a1)
                            (angle-arm-1 a2))
       (direction-opposite? (angle-arm-2 a1)
                            (angle-arm-2 a2))))
