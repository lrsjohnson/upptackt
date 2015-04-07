;;; Measurements

(define (distance p1 p2)
  (sqrt (+ (square (- (point-x p1)
                      (point-x p2)))
           (square (- (point-y p1)
                      (point-y p2))))))

;;; Sign of distance is positive if the point is to the left of
;;; the line direction and negative if to the right.
(define (signed-distance-to-line point line)
  (let ((p1 (line-p1 line))
        (p2 (line-p2 line)))
    (let ((x0 (point-x point))
          (y0 (point-y point))
          (x1 (point-x p1))
          (y1 (point-y p1))
          (x2 (point-x p2))
          (y2 (point-y p2)))
      (/ (+ (- (* x0 (- y2 y1)))
            (* y0 (- x2 x1))
            (- (* x2 y1))
            (* y2 x1))
         (* 1.0
            (sqrt (+ (square (- y2 y1))
                     (square (- x2 x1)))))))))

(define (distance-to-line point line)
  (abs (signed-distance-to-line point line)))

(define (angle-measure a)
  (let* ((d1 (angle-arm-1 a))
         (d2 (angle-arm-2 a)))
    (subtract-directions d2 d1)))

;;; Created elements

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
