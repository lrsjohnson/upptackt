(define (debug-figure)
  (let-geo* ((a (random-point))
             (b (random-point))
             (s (make-segment a b))
             (m (segment-midpoint s))
             (pl (perpendicular
                  (line-from-points a b)
                  m)))
            (figure a b s m pl)))

(define (debug-figure-2)
  (let-geo* ((s (random-segment))
             (a (segment-endpoint-1 s))
             (b (segment-endpoint-2 s))
             (m (segment-midpoint s)))
            (figure a b s m)))



(define (demo-figure)
  (let-geo* ((a (random-point))
             (b (random-point))
             (c (random-point))

             (d (midpoint a b))
             (e (midpoint a c))
             (f (midpoint b c))

             (l1 (perpendicular (line-from-points a b) d))
             (l2 (perpendicular (line-from-points a c) e))
             (l3 (perpendicular (line-from-points b c) f))

             (i1 (intersect-lines l1 l2))
             (i2 (intersect-lines l1 l3))

             (cir (circle-from-points i1 a)))

            (figure
             (make-segment a b)
             (make-segment b c)
             (make-segment a c)
             a b c l1 l2 l3 cir
             i1 i2)))

(define (circle-test)
  (let* ((a (random-point))
         (b (random-point))
         (d (distance a b))
         (r (rand-range
             (* d 0.5)
             (* d 1)))
         (c1 (circle a r))
         (c2 (circle b r))
         (cd (intersect-circles c1 c2))
         (c (car cd))
         (d (cadr cd)))
    (figure a b #|c1 c2|# c d
            (make-segment a c)
            (make-segment a d)
            (make-segment b c)
            (make-segment b d))))

(define (line-test)
  (let* ((a (random-point))
         (b (random-point))
         (c (random-point))
         (d (random-point))
         (l1 (line-from-points a b))
         (l2 (line-from-points c d))
         (e (intersect-lines l1 l2))
         (f (point-on-line l1))
         (cir (circle-from-points e f)))
    (figure a b c d l1 l2 e f cir)))

(define (angle-test)
  (let* ((a (random-point))
         (b (random-point))
         (c (random-point))
         (a-1 (smallest-angle (angle a b c)))
         (a-2 (smallest-angle (angle b c a)))
         (a-3 (smallest-angle (angle c a b)))
         (l1 (angle-bisector a-1))
         (l2 (angle-bisector a-2))
         (l3 (angle-bisector a-3))
         (center-point
          (intersect-lines (ray->line l1)
                           (ray->line l2)))
         (radius-line
          (perpendicular (line-from-points b c)
                         center-point))
         (radius-point
          (intersect-lines radius-line
                           (line-from-points b c)))
         (cir (circle-from-points
               center-point
               radius-point))
         (pb1 (perpendicular-bisector
               (make-segment a b)))
         (pb2 (perpendicular-bisector
               (make-segment b c)))
         (pb-center (intersect-lines pb1 pb2))
         (circum-cir (circle-from-points
                      pb-center
                      a)))
    (figure a b c cir a-1 a-2 a-3
            pb-center
            circum-cir
            center-point
            (make-segment a b)
            (make-segment a c)
            (make-segment b c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run commands

(define current-figure demo-figure)

(define c
  (if (environment-bound? (the-environment) 'c)
      c
      (canvas)))

(define *num-inner-loop* 5)
(define *num-outer-loop* 5)


(define (run-figure current-figure-proc)
  (display "\n")
  (let outer-lp ((outer-num-remaining *num-outer-loop*))
    (let ((analysis-data (make-analysis-collector)))
      (reset-randomness)
      (let inner-lp ((num-remaining *num-inner-loop*))
        (let ((current-figure (current-figure-proc)))
          (draw-figure current-figure c)
          (let ((analysis-results (analyze-figure current-figure)))
            (save-results analysis-results analysis-data))
          (sleep-current-thread 1)
          (next-instance)
          (cond  ((> num-remaining 1)
                  (inner-lp (- num-remaining 1)))
                 ((next-wiggle-instance)
                  (inner-lp *num-inner-loop*)))))
      (display "--- Results ---\n")
      (print-analysis-results analysis-data)
      (if (> outer-num-remaining 1)
          (outer-lp (- outer-num-remaining 1))))))

(define interesting-figures
  (list demo-figure
        linear-pair
        vertical-angles
        corresponding-angles
        cyclic-quadrilateral))

(define (r)
  (for-each (lambda (figure)
             (run-figure figure))
            interesting-figures)
  'done)

;(r)
