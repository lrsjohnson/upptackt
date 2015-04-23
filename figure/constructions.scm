;;; constructions.scm --- Constructions

;;; Commentary:

;; Ideas:
;; - Various logical constructions that can be peformed on elements
;; - Some higher-level constructions...

;; Future:
;; - More constructions?
;; - Separation between compass/straightedge and compound?
;; - Experiment with higher-level vs. learned constructions

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;; Segment Constructions ;;;;;;;;;;;;;;;;;;;;;;;;

(define (midpoint p1 p2)
  (let ((newpoint
         (make-point (avg (point-x p1)
                          (point-x p2))
                     (avg (point-y p1)
                          (point-y p2)))))
    (with-dependency
     `(midpoint ,(element-dependency p1) ,(element-dependency p2))
     newpoint)))

(define (segment-midpoint s)
  (let ((p1 (segment-endpoint-1 s))
        (p2 (segment-endpoint-2 s)))
    (with-dependency
     `(segment-midpoint ,(element-dependency s))
     (midpoint p1 p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: Where to put these?
(define (on-segment? p seg)
  (let ((p1 (segment-endpoint-1 seg))
        (p2 (segment-endpoint-2 seg)))
    (let ((d1 (distance p p1))
          (d2 (distance p p2))
          (d3 (distance p1 p2)))
      (close-enuf? (+ d1 d2) d3))))

;;; TODO: Fix for lines / segments
(define (on-line? p l)
  (on-segment?
   p
   (make-segment (line-p1 l)
                 (line-p2 l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Intersections ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
              (make-point px py)))))))

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
          (list (make-point (+ x1 dx)
                            (+ y1 dy))
                (make-point (- x1 dx)
                            (- y1 dy)))))))

;;;;;;;;;;;;;;;;;;;;;;; Construction of lines ;;;;;;;;;;;;;;;;;;;;;;;;

(define (perpendicular linear-element point)
  (let* ((direction (->direction linear-element))
         (rotated-direction (rotate-direction-90 direction)))
    (make-line point rotated-direction)))

(define (perpendicular-bisector segment)
  (let ((midpt (segment-midpoint segment)))
    (perpendicular (segment->line segment)
                   midpt)))

(define (angle-bisector a)
  (let* ((d1 (angle-arm-1 a))
         (d2 (angle-arm-2 a))
         (vertex (angle-vertex a))
         (radians (angle-measure a))
         (half-angle (/ radians 2))
         (new-direction (add-to-direction d2 half-angle)))
    (make-ray vertex new-direction)))
