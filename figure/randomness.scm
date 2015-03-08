;;; Utilities

;;;  Random Scalars
(define (rand-range min max)
  (+ min (random (* 1.0 (- max min)))))

(define (rand-angle-measure)
  (rand-range 0 (* 2 pi)))

;;; Random unit vector
(define (random-direction)
  (let ((theta (rand-angle-measure)))
    (unit-vec-from-angle theta)))

;;; Random Elements
(define (random-point)
  (point (rand-range -0.8 0.8)
         (rand-range -0.8 0.8)))

(define (random-line)
  (let ((p (random-point)))
    (line-through-point p)))

(define (line-through-point p)
  (let ((v (random-direction)))
    (line-from-point-vec p v)))

(define (ray-from-point p)
  (let ((v (random-direction)))
    (ray-from-point-vec p v)))

(define (horizontal-line)
  (let ((p (random-point))
        (v (make-vec 1 0)))
    (line-from-point-vec p v)))

(define (vertical-line)
  (let ((p (random-point))
        (v (make-vec 0 1)))
    (line-from-point-vec p v)))

;;;  Points on Random Elements
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
        (angle (rand-angle-measure)))
    (point (+ (point-x center)
              (* radius (cos angle)))
           (+ (point-y center)
              (* radius (sin angle))))))
