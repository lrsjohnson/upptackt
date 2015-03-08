;; Figure abstractions

;;; Tag Helper
(define ((tag-predicate tag) x)
  (and (pair? x)
       (eq? (car x) tag)))

;;; Assign name
(define (set-element-name! element name)
  (eq-put! element 'name name))

(define (element-name element)
  (eq-get element 'name))

;;; Structures For computation, not display
(define (make-vec dx dy)
  (list 'vec dx dy))
(define (vec-x d)
  (cadr d))
(define (vec-y d)
  (caddr d))
(define vec? (tag-predicate 'vec))

(define (rotate-vec-90 v)
  (let ((dx (vec-x v))
        (dy (vec-y v)))
    (make-vec (- dy) dx)))

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

(define (unit-vec-from-angle theta)
  (make-vec (cos theta) (sin theta)))

(define (vec-magnitude v)
  (let ((dx (vec-x v))
        (dy (vec-y v)))
    (sqrt (+ (square dx) (square dy)))))

(define (unit-vec v)
  (scale-vec v (/ (vec-magnitude v))))

(define (scale-vec-to-dist v dist)
  (scale-vec (unit-vec v) dist))

(define (vec-to-angle v)
  (let ((dx (vec-x v))
        (dy (vec-y v)))
    (atan dy dx)))

(define (float-mod num mod)
  (- num
     (* (floor (/ num mod))
        mod)))

(define (fix-angle-0-2pi a)
  (float-mod a (* 2 pi)))

(define (rad->deg rad)
  (* (/ rad (* 2 pi)) 360))

;;; Figure Primitives
(define (point x y)
  (list 'point x y))
(define (point-x p)
  (cadr p))
(define (point-y p)
  (caddr p))
(define point? (tag-predicate 'point))

(define (point-equal? p1 p2)
  (and (close-enuf? (point-x p1)
                    (point-x p2))
       (close-enuf? (point-y p1)
                    (point-y p2))))

;;; Vec from p1 to p2
(define (sub-points p2 p1)
  (let ((x1 (point-x p1))
        (x2 (point-x p2))
        (y2 (point-y p2))
        (y1 (point-y p1)))
    (make-vec (- x2 x1)
              (- y2 y1))))

(define (add-to-point p vec)
  (let ((x (point-x p))
        (y (point-y p))
        (dx (vec-x vec))
        (dy (vec-y vec)))
    (point (+ x dx)
           (+ y dy))))

(define (segment p1 p2)
  (list 'segment p1 p2))
(define (segment-p1 segment)
  (cadr segment))
(define (segment-p2 segment)
  (caddr segment))
(define segment? (tag-predicate 'segment))

(define (line p1 p2)
  (list 'line p1 p2))
(define (line-p1 line)
  (cadr line))
(define (line-p2 line)
  (caddr line))
(define line? (tag-predicate 'line))

(define (ray p1 p2)
  (list 'ray p1 p2))
(define (ray-p1 ray)
  (cadr ray))
(define (ray-p2 ray)
  (caddr ray))
(define ray? (tag-predicate 'ray))

(define (circle center radius)
  (list 'circle center radius))
(define (circle-center c)
  (cadr c))
(define (circle-radius c)
  (caddr c))
(define circle? (tag-predicate 'circle))

(define (avg a b)
  (/ (+ a b) 2))

;;; Angles

;;; v1 and v2 are vectors in the directions of the angle arms
(define (make-angle v1 vertex v2)
  (list 'angle v1 vertex v2))

(define (angle-from-points p1 vertex p2)
  (let ((v1 (sub-points p1 vertex))
        (v2 (sub-points p2 vertex)))
    (make-angle v1 vertex v2)))

(define (angle-arm-1 a)
  (cadr a))
(define (angle-vertex a)
  (caddr a))
(define (angle-arm-2 a)
  (cadddr a))
(define angle? (tag-predicate 'angle))

(define (reverse-angle a)
  (let ((a1 (angle-arm-1 a))
        (v (angle-vertex a))
        (a2 (angle-arm-2 a)))
    (make-angle a2 v a1)))

;;; Alternate, helper constructors
(define (line-from-point-vec p vec)
  (let ((p2 (add-to-point p vec)))
    (line p p2)))

(define (line-through-point p)
  (let ((v (random-direction)))
    (line-from-point-vec p v)))

(define (ray-from-point-vec p vec)
  (let ((p2 (add-to-point p vec)))
    (ray p p2)))

(define (ray-from-point p)
  (let ((v (random-direction)))
    (ray-from-point-vec p v)))

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

;;: Construct Rays

(define (ray-from-point-vector p v)
  (let ((p2 (add-to-point p v)))
    (ray p p2)))

(define (ray-from-angle-arm-1 a)
  (ray-from-point-vector (angle-vertex a)
                         (angle-arm-1 a)))
(define (ray-from-angle-arm-2 a)
  (ray-from-point-vector (angle-vertex a)
                         (angle-arm-2 a)))

;;; Transformations

(define flip (make-generic-operation 1 'flip))

(define (flip-line l)
  (line (line-p2 l) (line-p1 l)))
(defhandler flip flip-line line?)

;;; Conversions, extending shorter segments

(define (smallest-angle a)
  (if (> (angle-measure a) pi)
      (reverse-angle a)
      a))

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
  (sub-points (ray-p2 r)
              (ray-p1 r)))

;;; Constructions
(define (midpoint p1 p2)
  (point (avg (point-x p1)
                   (point-x p2))
              (avg (point-y p1)
                   (point-y p2))))

(define (segment-midpoint s)
  (let ((p1 (segment-p1 s))
        (p2 (segment-p2 s)))
    (midpoint p1 p2)))

(define (on-line? p l)
  (let ((p1 (line-p1 l))
        (p2 (line-p2 l)))
    (let ((d1 (distance p p1))
          (d2 (distance p p2))
          (d3 (distance p1 p2)))
      (close-enuf? (+ d1 d2) d3))))

(define (intersect-lines line1 line2)
  (let ((p1 (line-p1 line1))
        (p2 (line-p2 line1))
        (p3 (line-p1 line2))
        (p4 (line-p2 line2)))
    (let ((x1 (point-x p1))
          (y1 (point-y p1))
          (x2 (point-x p2))
          (y2 (point-y p2))
          (x3 (point-x p3))
          (y3 (point-y p3))
          (x4 (point-x p4))
          (y4 (point-y p4)))
      (let* ((denom
              (det (det x1 1 x2 1)
                   (det y1 1 y2 1)
                   (det x3 1 x4 1)
                   (det y3 1 y4 1)))
             (num-x
              (det (det x1 y1 x2 y2)
                   (det x1  1 x2  1)
                   (det x3 y3 x4 y4)
                   (det x3  1 x4  1)))
             (num-y
              (det (det x1 y1 x2 y2)
                   (det y1  1 y2  1)
                   (det x3 y3 x4 y4)
                   (det y3  1 y4  1))))
        (if (= denom 0)
            (error "lines don't intersect")
            (let
                ((px (/ num-x denom))
                 (py (/ num-y denom)))
              (point px py)))))))

;;; http://mathforum.org/library/drmath/view/51836.html
(define (intersect-circles cir1 cir2)
  (let* ((c1 (circle-center cir1))
         (c2 (circle-center cir2))
         (r (circle-radius cir1))
         (s (circle-radius cir2))
         (a (point-x c1))
         (b (point-y c1))
         (c (point-x c2))
         (d (point-y c2))
         (e (- c a))
         (f (- d b))
         (p (sqrt (+ (square e)
                     (square f))))
         (k (/ (- (+ (square p) (square r))
                  (square s))
               (* 2 p))))
    (if (> k r)
        (error "Circle's don't intersect")
        (let* ((t (sqrt (- (square r)
                           (square k))))
               (x1 (+ a (/ (* e k) p)))
               (y1 (+ b (/ (* f k) p)))
               (dx (/ (* f t) p))
               (dy (- (/ (* e t) p))))
          (list (point (+ x1 dx)
                       (+ y1 dy))
                (point (- x1 dx)
                       (- y1 dy)))))))

(define (circle-from-points center radius-point)
  (circle center
          (distance center radius-point)))

(define (perpendicular l point)
  (let* ((p1 (line-p1 l))
         (p2 (line-p2 l))
         (v (sub-points p2 p1))
         (rotated-v (rotate-vec-90 v))
         (new-p (add-to-point point rotated-v)))
    (line point new-p)))

(define (perpendicular-bisector segment)
  (let ((midpt (segment-midpoint segment)))
    (perpendicular (segment->line segment)
                   midpt)))

;; Angle -> Ray
(define (angle-bisector a)
  (let* ((a1 (angle-arm-1 a))
         (a2 (angle-arm-2 a))
         (vertex (angle-vertex a))
         (radians (angle-measure a))
         (half-angle (/ radians 2))
         (new-angle (measured-angle-ccw
                     (add-to-point vertex a2)
                     vertex
                     half-angle)))
    (ray vertex (angle-p2 new-angle))))

;;; Measurement-directed constructions
;;; TODO: Wrap in "compass/straightedge" ones, or replace with compass versions

(define (measured-point-on-ray r dist)
  (let* ((p1 (ray-p1 r))
         (p2 (ray-p2 r))
         (v (sub-points p1 p2))
         (scaled-v (scale-vec-to-dist v dist)))
    (add-to-point p1 scaled-v)))

(define (measured-angle-ccw p1 vertex radians)
  (let* ((v1 (sub-points p1 vertex))
         (v-rotated (rotate-vec v (- radians))))
    (angle v1 vertex v-rotated)))

(define measured-angle measured-angle-ccw)

(define (measured-angle-cw p1 vertex radians)
  (reverse-angle (measured-angle-ccw p1 vertex (- radians))))

;;; Measurements

(define (distance p1 p2)
  (sqrt (+ (square (- (point-x p1)
                      (point-x p2)))
           (square (- (point-y p1)
                      (point-y p2))))))

(define (angle-measure a)
  (let* ((vertex (angle-vertex a))
         (arm1 (angle-arm-1 a))
         (arm2 (angle-arm-2 a))
         (angle-start (vec-to-angle arm1))
         (angle-end (vec-to-angle arm2)))
    (fix-angle-0-2pi (- angle-end
                        angle-start))))

(define max-dist 2)

;;; Figure Structure

(define (figure . elements)
  (cons 'figure elements))
(define (figure-elements figure)
  (cdr figure))
(define figure? (tag-predicate 'figure))

(define (figure-filter predicate figure)
  (filter predicate (figure-elements figure)))

;;; Utilities

(define pi (* 4 (atan 1)))

(define (rand-range min max)
  (+ min (random (* 1.0 (- max min)))))

(define (det a11 a12 a21 a22)
  (- (* a11 a22) (* a12 a21)))

;;; Randomness / arbitrary choices
(define (random-point)
  (point (rand-range -0.8 0.8)
         (rand-range -0.8 0.8)))

;;; "Unit Vector"
(define (random-direction)
  (let ((theta (rand-range 0 (* 2 pi))))
    (unit-vec-from-angle theta)))

(define (point-on-segment seg)
  (let* ((p1 (segment-p1 seg))
         (p2 (segment-p2 seg))
         (t (rand-range 0.0 1.0))
         (v (sub-points p2 p1)))
    (add-to-point p1 (scale-vec v t))))

(define (point-on-line l)
  (let* ((p1 (line-p1 l))
         (p2 (line-p2 l))
         (seg (extend-to-max-segment p1 p2))
         (sp1 (segment-p1 seg))
         (sp2 (segment-p2 seg))
         (t (rand-range 0.0 1.0))
         (v (sub-points sp2 sp1)))
    (add-to-point sp1 (scale-vec v t))))

(define (point-on-ray r)
  (let* ((p1 (ray-p1 r))
         (p2 (ray-p2 r))
         (seg (ray-extend-to-max-segment p1 p2))
         (sp1 (segment-p1 seg))
         (sp2 (segment-p2 seg))
         (t (rand-range 0.0 1.0))
         (v (sub-points sp2 sp1)))
    (add-to-point sp1 (scale-vec v t))))

(define (point-on-circle c)
  (let ((center (circle-center c))
        (radius (circle-radius c))
        (angle (rand-range 0 (* 2 pi))))
    (point (+ (point-x center)
              (* radius (cos angle)))
           (+ (point-y center)
              (* radius (sin angle))))))

(define (extend-to-max-segment p1 p2)
  (let ((x1 (point-x p1))
        (y1 (point-y p1))
        (x2 (point-x p2))
        (y2 (point-y p2)))
    (let ((dx (- x2 x1))
          (dy (- y2 y1)))
      (cond
       ((= 0 dx) (segment
                  (point x1 *g-min-y*)
                  (point x1 *g-max-y*)))
       ((= 0 dy) (segment
                  (point *g-min-x* y1)
                  (point *g-min-y* y1)))
       (else
        (let ((t-xmin (/ (- *g-min-x* x1) dx))
              (t-xmax (/ (- *g-max-x* x1) dx))
              (t-ymin (/ (- *g-min-y* y1) dy))
              (t-ymax (/ (- *g-max-y* y1) dy)))
          (let* ((sorted (sort (list t-xmin t-xmax t-ymin t-ymax) <))
                 (min-t (cadr sorted))
                 (max-t (caddr sorted))
                 (min-x (+ x1 (* min-t dx)))
                 (min-y (+ y1 (* min-t dy)))
                 (max-x (+ x1 (* max-t dx)))
                 (max-y (+ y1 (* max-t dy))))
            (segment (point min-x min-y)
                     (point max-x max-y)))))))))

(define (ray-extend-to-max-segment p1 p2)
  (let ((x1 (point-x p1))
        (y1 (point-y p1))
        (x2 (point-x p2))
        (y2 (point-y p2)))
    (let ((dx (- x2 x1))
          (dy (- y2 y1)))
      (cond
       ((= 0 dx) (segment
                  (point x1 *g-min-y*)
                  (point x1 *g-max-y*)))
       ((= 0 dy) (segment
                  (point *g-min-x* y1)
                  (point *g-min-y* y1)))
       (else
        (let ((t-xmin (/ (- *g-min-x* x1) dx))
              (t-xmax (/ (- *g-max-x* x1) dx))
              (t-ymin (/ (- *g-min-y* y1) dy))
              (t-ymax (/ (- *g-max-y* y1) dy)))
          (let* ((sorted (sort (list t-xmin t-xmax t-ymin t-ymax) <))
                 (min-t (cadr sorted))
                 (max-t (caddr sorted))
                 (min-x (+ x1 (* min-t dx)))
                 (min-y (+ y1 (* min-t dy)))
                 (max-x (+ x1 (* max-t dx)))
                 (max-y (+ y1 (* max-t dy))))
            (segment p1
                     (point max-x max-y)))))))))
