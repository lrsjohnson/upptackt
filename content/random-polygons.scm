;;; random-polygons.scm --- Random creation of polygons

;;; Commentary:

;; Ideas:
;; - Separated out polygons from other system-centric random proceudres
;; - These can be thought of as "user-provided" instead of system provided.

;; Future:
;; - More polygon types

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;; Random Triangles ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-equilateral-triangle)
  (let* ((s1 (random-segment))
         (s2 (rotate-about (segment-endpoint-1 s1)
                           (/ pi 3)
                           s1)))
    (with-dependency
     (make-random-dependency 'random-equilateral-triangle)
     (polygon-from-points
      (segment-endpoint-1 s1)
      (segment-endpoint-2 s1)
      (segment-endpoint-2 s2)))))

(define (random-right-triangle)
  (let* ((r1 (random-ray))
         (r2 (rotate-about (ray-endpoint r1)
                           (/ pi 2)
                           r1))
         (p1 (random-point-on-ray r1))
         (p2 (random-point-on-ray r2)))
    (with-dependency
     (make-random-dependency 'random-right-triangle)
     (polygon-from-points
      (ray-endpoint r1)
      p1 p2))))

(define (random-isoceles-triangle)
  (let* ((s1 (random-segment))
         (base-angle (rand-angle-measure))
         (s2 (rotate-about (segment-endpoint-1 s1)
                           base-angle
                           s1)))
    (with-dependency
     (make-random-dependency 'random-isoceles-triangle)
     (polygon-from-points
      (segment-endpoint-1 s1)
      (segment-endpoint-2 s1)
      (segment-endpoint-2 s2)))))

(define (random-right-isoceles-triangle)
  (let* ((s1 (random-segment))
         (s2 (rotate-about (segment-endpoint-1 s1)
                           (/ pi 2)
                           s1)))
    (with-dependency
     (make-random-dependency 'random-right-isoceles-triangle)
     (polygon-from-points
      (segment-endpoint-1 s1)
      (segment-endpoint-2 s1)
      (segment-endpoint-2 s2)))))
;;;;;;;;;;;;;;;;;;;;;;; Random Quadrilaterals ;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-square)
  (let* ((s1 (random-segment))
         (p1 (segment-endpoint-1 s1))
         (p2 (segment-endpoint-2 s1))
         (p3 (rotate-about p2
                           (- (/ pi 2))
                           p1))
         (p4 (rotate-about p1
                           (/ pi 2)
                           p2)))
    (with-dependency
     (make-random-dependency 'random-square)
     (polygon-from-points p1 p2 p3 p4))))

(define (random-rectangle)
  (let* ((r1 (random-ray))
         (p1 (ray-endpoint r1))
         (r2 (rotate-about (ray-endpoint r1)
                           (/ pi 2)
                           r1))
         (p2 (random-point-on-ray r1))
         (p4 (random-point-on-ray r2))
         (p3 (add-to-point
              p2
              (sub-points p4 p1))))
    (with-dependency
     (make-random-dependency 'random-rectangle)
     (polygon-from-points
      p1 p2 p3 p4))))

(define (random-parallelogram)
  (let* ((r1 (random-ray))
         (p1 (ray-endpoint r1))
         (r2 (rotate-about (ray-endpoint r1)
                           (rand-angle-measure)
                           r1))
         (p2 (random-point-on-ray r1))
         (p4 (random-point-on-ray r2))
         (p3 (add-to-point
              p2
              (sub-points p4 p1))))
    (with-dependency
     (make-random-dependency 'random-parallelogram)
     (polygon-from-points p1 p2 p3 p4))))

(define (random-kite)
  (let* ((r1 (random-ray))
         (p1 (ray-endpoint r1))
         (r2 (rotate-about (ray-endpoint r1)
                           (rand-obtuse-angle-measure)
                           r1))
         (p2 (random-point-on-ray r1))
         (p4 (random-point-on-ray r2))
         (p3 (reflect-about-line
              (line-from-points p2 p4)
              p1)))
    (with-dependency
     (make-random-dependency 'random-parallelogram)
     (polygon-from-points p1 p2 p3 p4))))

(define (random-rhombus)
  (let* ((s1 (random-segment))
         (p1 (segment-endpoint-1 s1))
         (p2 (segment-endpoint-2 s1))
         (p4 (rotate-about p1
                           (rand-angle-measure)
                           p2))
         (p3 (add-to-point
              p2
              (sub-points p4 p1))))
    (with-dependency
     (make-random-dependency 'random-rhombus)
     (polygon-from-points p1 p2 p3 p4))))

(define (random-trapezoid)
  (let* ((r1 (random-ray))
         (r2 (translate-randomly r1))
         (p1 (ray-endpoint r1))
         (p2 (random-point-on-ray r1))
         (p3 (random-point-on-ray r2))
         (p4 (ray-endpoint r2)))
    (with-dependency
     (make-random-dependency 'random-trapezoid)
     (polygon-from-points p1 p2 p3 p4))))

(define (random-orthodiagonal-quadrilateral)
  (let* ((r1 (random-ray))
         (r2 (rotate-about
              (ray-endpoint r1)
              (/ pi 2)
              r1))
         (r3 (reverse-ray r1))
         (r4 (reverse-ray r2))
         (a (random-point-on-ray r1))
         (b (random-point-on-ray r2))
         (c (random-point-on-ray r3))
         (d (random-point-on-ray r4)))
    (with-dependency
     (make-random-dependency 'random-orthodiagonal-quadrilateral)
     (polygon-from-points a b c d))))

(define (random-cyclic-quadrilateral)
  (let ((cir (random-circle)))
    (let lp ()
      (let ((points (n-random-points-on-circle-ccw cir 4)))
        (if (points-non-overlapping? points)
            (apply polygon-from-points points)
            (lp))))))

(define (random-equidiagonal-quadrilateral)
  (let* ((s (random-segment))
         (p1 (random-point-on-segment s))
         (s-rotated (rotate-randomly-about p1 s))
         (p2 (random-point-on-segment s-rotated))
         (s2 (translate-by
              (sub-points p1 p2)
              s-rotated)))
    (polygon-from-points (segment-endpoint-1 s)
                         (segment-endpoint-1 s2)
                         (segment-endpoint-2 s)
                         (segment-endpoint-2 s2))))

(define (random-isoceles-trapezoid)
  (let* ((a1 (random-obtuse-angle))
         (p1 (angle-vertex a1))
         (r1 (ray-from-arm-1 a1))
         (r2 (ray-from-arm-2 a1))
         (p4 (random-point-on-ray r2))
         (p2 (random-point-on-ray r1))
         (s (make-segment p1 p2))
         (pb (perpendicular-bisector s))
         (p3 (reflect-about-line pb p4)))
    (polygon-from-points p1 p2 p3 p4)))

(define (random-3-equal-trapezoid)
  (let* ((a1 (random-obtuse-angle))
         (p1 (angle-vertex a1))
         (r1 (ray-from-arm-1 a1))
         (r2 (ray-from-arm-2 a1))
         (p2 (random-point-on-ray r1))
         (p4 (measured-point-on-ray
              r2 (distance p1 p2)))
         (s (make-segment p1 p2))
         (pb (perpendicular-bisector s))
         (p3 (reflect-about-line pb p4)))
    (polygon-from-points p1 p2 p3 p4)))
