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
