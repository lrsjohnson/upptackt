;;; Tag Helper
(define ((tag-predicate tag) x)
  (and (pair? x)
       (eq? (car x) tag)))

;;; Structures For computation, not display
(define (make-vec dx dy)
  (list 'vec dx dy))
(define (vec-x d)
  (cadr d))
(define (vec-y d)
  (caddr d))
(define vec? (tag-predicate 'vec))

;;; Figure Primitives
(define (point x y)
  (list 'point x y))
(define (point-x p)
  (cadr p))
(define (point-y p)
  (caddr p))
(define point? (tag-predicate 'point))

(define (rotate-vec-90 d)
  (let ((dx (vec-x d))
        (dy (vec-y d)))
    (make-vec (- dy) dx)))

;;; Vec from p1 to p2
(define (sub-points p2 p1)
  (let ((x1 (point-x p1))
        (y1 (point-y p1))
        (x2 (point-x p2))
        (y2 (point-y p2)))
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

;;; Constructions
(define (midpoint p1 p2)
  (point (avg (point-x p1)
                   (point-x p2))
              (avg (point-y p1)
                   (point-y p2))))

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

(define (segment->line segment)
  (line (segment-p1 segment)
        (segment-p2 segment)))

(define (perpendicular l point)
  (let* ((p1 (line-p1 l))
         (p2 (line-p2 l))
         (v (sub-points p2 p1))
         (rotated-v (rotate-vec-90 v))
         (new-p (add-to-point point rotated-v)))
    (line point new-p)))


;;; Measurements

(define (distance p1 p2)
  (sqrt (+ (square (- (point-x p1)
                      (point-x p2)))
           (square (- (point-y p1)
                      (point-y p2))))))

(define max-dist 2)

;;; Figure Structure

(define (figure . elements)
  (cons 'figure elements))
(define (figure-elements figure)
  (cdr figure))
(define figure? (tag-predicate 'figure))

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

(define (point-on-circle c)
  (let ((center (circle-center c))
        (radius (circle-radius c))
        (angle (rand-range 0 (* 2 pi))))
    (point (+ (point-x center)
              (* radius (cos angle)))
           (+ (point-y center)
              (* radius (sin angle))))))
