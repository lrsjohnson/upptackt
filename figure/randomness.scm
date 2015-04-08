;;; Utilities

;;;  Base: Random Scalars
(define (internal-rand-range min-v max-v)
  (let ((interval-size (max *machine-epsilon* (- max-v min-v))))
    (+ min-v (random (* 1.0 interval-size)))))

;;; Global for if its the first construction of the
(define *first-construction* #t)
(define *random-call-count* 0)
(define *max-random-call-count* 0)
(define *wiggle-random-call-count* 0)
(define *random-values-table* (make-key-weak-eq-hash-table))

(define (reset-randomness)
  (set! *first-construction* #t)
  (set! *random-call-count* 0)
  (set! *max-random-call-count* 0)
  (set! *wiggle-random-call-count* 0)
  (set! *random-values-table* (make-key-weak-eq-hash-table)))

(define (next-wiggle-instance)
  (let ((result
         (if (< *wiggle-random-call-count*
                (- *max-random-call-count* 1))
             (begin (set! *wiggle-random-call-count*
                          (+ 1 *wiggle-random-call-count*))
                    #t)
             #f)))
    (next-instance)
    result))

(define (next-instance)
  (set! *first-construction* #f)
  (set! *random-call-count* 0))

(define (save-random-value v)
  (hash-table/put! *random-values-table* *random-call-count* v)
  (set! *random-call-count*
        (+ 1 *random-call-count*)))

(define (get-random-value)
  (hash-table/get *random-values-table* *random-call-count* #f))

(define (should-wiggle)
  (and (not *first-construction*) (= *random-call-count* *wiggle-random-call-count*)))


(define *wiggle-ratio* 0.15)

;;; Will return floats even if passed integers
(define (rand-range min max)
  (let* ((range-size (- max min))
         (wiggle-amount (* range-size *wiggle-ratio*))
         (v (persist-value (internal-rand-range min (- max wiggle-amount)))))
    (animate-range v (+ v wiggle-amount))))

;;; Random Values - distances, angles

(define (rand-theta)
  (rand-range 0 (* 2 pi)))

(define (rand-angle-measure)
  (rand-range 0 pi))

;;; Random unit vector
(define (random-direction)
  (let ((theta (rand-theta)))
    (make-direction theta)))

;;; Random Elements
(define (random-point)
  (make-point (rand-range -0.8 0.8)
              (rand-range -0.8 0.8)))

(define (random-line)
  (let ((p (random-point)))
    (line-through-point p)))

(define (random-segment)
  (let ((p1 (random-point))
        (p2 (random-point)))
    (make-segment p1 p2)))

(define (random-circle)
  (let ((pr1 (random-point))
        (pr2 (random-point)))
    (circle-from-points (midpoint pr1 pr2) pr1)))

(define (line-through-point p)
  (let ((v (random-direction)))
    (line-from-point-direction p v)))

(define (ray-from-point p)
  (let ((v (random-direction)))
    (ray-from-point-direction p v)))

(define (horizontal-line)
  (let ((p (random-point))
        (v (make-vec 1 0)))
    (line-from-point-vec p v)))

(define (vertical-line)
  (let ((p (random-point))
        (v (make-vec 0 1)))
    (line-from-point-vec p v)))

(define (random-angle)
  (let ((v (random-point))
        (d1 (random-direction))
        (d2 (random-direction)))
    (smallest-angle (make-angle d1 v d2))))

;;;  Points on Random Elements
(define (point-on-segment seg)
  (let* ((p1 (segment-endpoint-1 seg))
         (p2 (segment-endpoint-2 seg))
         (t (rand-range 0.0 1.0))
         (v (sub-points p2 p1)))
    (add-to-point p1 (scale-vec v t))))

;;; TODO: Fix this for new construction
(define (point-on-line l)
  (let* ((p1 (line-p1 l))
         (p2 (line-p2 l))
         (seg (extend-to-max-segment p1 p2))
         (sp1 (segment-endpoint-1 seg))
         (sp2 (segment-endpoint-2 seg))
         (t (rand-range 0.0 1.0))
         (v (sub-points sp2 sp1)))
    (add-to-point sp1 (scale-vec v t))))

(define (point-on-ray r)
  (let* ((p1 (ray-endpoint r))
         (dir (ray-direction r))
         (p2 (add-to-point p1 (vec-from-direction dir)))
         (seg (ray-extend-to-max-segment p1 p2))
         (sp1 (segment-endpoint-1 seg))
         (sp2 (segment-endpoint-2 seg))
         (t (rand-range 0.0 1.0))
         (v (sub-points sp2 sp1)))
    (add-to-point sp1 (scale-vec v t))))

(define (point-on-circle c)
  (let ((center (circle-center c))
        (radius (circle-radius c))
        (angle (rand-theta)))
    (make-point (+ (point-x center)
                   (* radius (cos angle)))
                (+ (point-y center)
                   (* radius (sin angle)))))) ;; TODO  Cleanup

(define (random-circle-radius c)
  (let ((center (circle-center c))
        (radius (circle-radius c))
        (angle (random-direction)))
    (let ((radius-vec
           (scale-vec (unit-vec-from-direction)
                      radius)))
      (let ((radius-point (add-to-point center radius-vec)))
        (segment center radius-point)))))
