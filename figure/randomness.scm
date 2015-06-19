;;; randomness.scm --- Random creation of elements

;;; Commentary:

;; Ideas:
;; - Random points, segments, etc. essential to system
;; - Separated out animation / persistence across frames

;; Future:
;; - Better random support
;; - Maybe separating out "definitions" (random square, etc.)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;; Base: Random Scalars ;;;;;;;;;;;;;;;;;;;;;;;;

(define (internal-rand-range min-v max-v)
  (if (close-enuf? min-v max-v)
      (error "range is too close for rand-range"
             (list min-v max-v))
      (let ((interval-size (max *machine-epsilon* (- max-v min-v))))
        (persist-value (+ min-v (random (* 1.0 interval-size)))))))

(define (safe-internal-rand-range min-v max-v)
  (let ((interval-size (max 0 (- max-v min-v))))
    (internal-rand-range
     (+ min-v (* 0.1 interval-size))
     (+ min-v (* 0.9 interval-size)))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Animated Ranges ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *wiggle-ratio* 0.15)

;;; Will return floats even if passed integers
;;; TODO: Rename to animated?
(define (rand-range min max)
  (let* ((range-size (- max min))
         (wiggle-amount (* range-size *wiggle-ratio*))
         (v (internal-rand-range min (- max wiggle-amount))))
    (animate-range v (+ v wiggle-amount))))

(define (safe-rand-range min-v max-v)
  (let ((interval-size (max 0 (- max-v min-v))))
    (rand-range
     (+ min-v (* 0.1 interval-size))
     (+ min-v (* 0.9 interval-size)))))

;;; Random Values - distances, angles

(define (rand-theta)
  (rand-range 0 (* 2 pi)))

(define (rand-angle-measure)
  (rand-range (* pi 0.05) (* .95 pi)))

(define (rand-obtuse-angle-measure)
  (rand-range (* pi 0.55) (* .95 pi)))

(define (random-direction)
  (let ((theta (rand-theta)))
    (make-direction theta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Random Points ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *point-wiggle-radius* 0.05)
(define (random-point)
  (let ((x (internal-rand-range -0.8 0.8))
        (y (internal-rand-range -0.8 0.8)))
    (random-point-around (make-point x y))))

(define (random-point-around p)
  (let ((x (point-x p))
        (y (point-y p)))
    (let ((theta (internal-rand-range 0 (* 2 pi)))
          (d-theta (animate-range 0 (* 2 pi))))
      (let ((dir (make-direction (+ theta d-theta))))
        (with-dependency
         (make-random-dependency 'random-point)
         (add-to-point
          (make-point x y)
          (vec-from-direction-distance dir *point-wiggle-radius*)))))))

;;; TODO: Maybe separate out reflection about line?
(define (random-point-left-of-line line)
  (let* ((p (random-point))
         (d (signed-distance-to-line p line))
         (v (rotate-vec-90
             (unit-vec-from-direction
              (line-direction line)))))
    (if (> d 0)
        p
        (add-to-point p (scale-vec v (* 2 (- d)))))))

(define (random-point-between-rays r1 r2)
  (let ((offset-vec (sub-points (ray-endpoint r2)
                            (ray-endpoint r1))))
    (let ((d1 (ray-direction r1))
          (d2 (ray-direction r2)))
      (let ((dir-difference (subtract-directions d2 d1)))
        (let ((new-dir (add-to-direction
                        d1
                        (internal-rand-range 0.05 dir-difference))))
          (random-point-around
           (add-to-point
            (add-to-point (ray-endpoint r1)
                          (vec-from-direction-distance
                           new-dir
                           (internal-rand-range 0.05 0.9)))
            (scale-vec offset-vec
                       (internal-rand-range 0.05 0.9)))))))))

(define (random-point-on-segment seg)
  (let* ((p1 (segment-endpoint-1 seg))
         (p2 (segment-endpoint-2 seg))
         (t (rand-range 0.05 1.0))
         (v (sub-points p2 p1)))
    (add-to-point p1 (scale-vec v t))))

;;; TODO: Fix this for new construction
(define (random-point-on-line l)
  (let* ((p1 (line-p1 l))
         (p2 (line-p2 l))
         (seg (extend-to-max-segment p1 p2))
         (sp1 (segment-endpoint-1 seg))
         (sp2 (segment-endpoint-2 seg))
         (t (rand-range 0.0 1.0))
         (v (sub-points sp2 sp1)))
    (add-to-point sp1 (scale-vec v t))))

(define (random-point-on-ray r)
  (let* ((p1 (ray-endpoint r))
         (dir (ray-direction r))
         (p2 (add-to-point p1 (unit-vec-from-direction dir)))
         (seg (ray-extend-to-max-segment p1 p2))
         (sp1 (segment-endpoint-1 seg))
         (sp2 (segment-endpoint-2 seg))
         (t (rand-range 0.05 1.0))
         (v (sub-points sp2 sp1)))
    (add-to-point sp1 (scale-vec v t))))


#|
(define (random-point-on-ray r)
  (random-point-on-segment
   (ray-extend-to-max-segment r)))
|#

(define (random-point-on-circle c)
  (let ((dir (random-direction)))
    (point-on-circle-in-direction c dir)))

(define (n-random-points-on-circle-ccw c n)
  (let* ((thetas
          (sort
           (make-initialized-list n (lambda (i) (rand-theta)))
           <)))
    (map (lambda (theta)
           (point-on-circle-in-direction
            c
            (make-direction theta)))
         thetas)))

;;;;;;;;;;;;;;;;;;;;;;; Random Linear Elements ;;;;;;;;;;;;;;;;;;;;;;;

(define (random-line)
  (let ((p (random-point)))
    (with-dependency
     (make-random-dependency 'random-line)
     (random-line-through-point p))))

(define (random-segment)
  (let ((p1 (random-point))
        (p2 (random-point)))
    (let ((seg (make-segment p1 p2)))
      (with-dependency
       (make-random-dependency 'random-segment)
       seg))))

(define (random-ray)
  (let ((p (random-point)))
    (random-ray-from-point p)))

(define (random-line-through-point p)
  (let ((v (random-direction)))
    (line-from-point-direction p v)))

(define (random-ray-from-point p)
  (let ((v (random-direction)))
    (ray-from-point-direction p v)))

(define (random-horizontal-line)
  (let ((p (random-point))
        (v (make-vec 1 0)))
    (line-from-point-vec p v)))

(define (random-vertical-line)
  (let ((p (random-point))
        (v (make-vec 0 1)))
    (line-from-point-vec p v)))

;;;;;;;;;;;;;;;;;;;;;;; Random Circle Elements ;;;;;;;;;;;;;;;;;;;;;;;

(define (random-circle-radius circle)
  (let ((center (circle-center circle))
        (radius (circle-radius circle))
        (angle (random-direction)))
    (let ((radius-vec
           (scale-vec (unit-vec-from-direction
                       (random-direction))
                      radius)))
      (let ((radius-point (add-to-point center radius-vec)))
        (make-segment center radius-point)))))

(define (random-circle)
  (let ((pr1 (random-point))
        (pr2 (random-point)))
    (circle-from-points (midpoint pr1 pr2) pr1)))

(define (random-angle)
  (let* ((v (random-point))
         (d1 (random-direction))
         (d2 (add-to-direction
              d1
              (rand-angle-measure))))
    (make-angle d1 v d2)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Random Polygons ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-n-gon n)
  (if (< n 3)
      (error "n must be > 3"))
  (let* ((p1 (random-point))
         (p2 (random-point)))
    (let ((ray2 (reverse-ray (ray-from-points p1 p2))))
      (let lp ((n-remaining (- n 2))
               (points (list p2 p1)))
        (if (= n-remaining 0)
            (apply polygon-from-points (reverse points))
            (lp (- n-remaining 1)
                (cons (random-point-between-rays
                       (reverse-ray (ray-from-points (car points)
                                                     (cadr points)))
                       ray2)
                      points)))))))

(define (random-polygon)
  (random-n-gon (+ 3 (random 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Random Triangles ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-triangle)
  (let* ((p1 (random-point))
         (p2 (random-point))
         (p3 (random-point-left-of-line (line-from-points p1 p2))))
    (with-dependency
     (make-random-dependency 'random-triangle)
     (polygon-from-points p1 p2 p3))))

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

;;;;;;;;;;;;;;;;;;;;;;; Random Quadrilaterals ;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-quadrilateral)
  (with-dependency
   (make-random-dependency 'random-quadrilateral)
   (random-n-gon 4)))

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
