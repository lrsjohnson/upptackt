(define (i-t-figure)
  (let-geo* (((t (a b c)) (random-isoceles-triangle)))
    (figure t)))


(define (midpoint-figure)
  (let-geo* (((s (a b)) (random-segment))
             (m (segment-midpoint s)))
    (figure s m)))

(define (random-rhombus-figure)
  (let-geo* (((r (a b c d)) (random-rhombus)))
    (figure r)))

;;; Other Examples:

(define (debug-figure)
  (let-geo* (((r (a b c d)) (random-parallelogram))
             (m1 (midpoint a b))
             (m2 (midpoint c d)))
    (figure r m1 m2)))

(define (demo-figure)
  (let-geo* (((t (a b c)) (random-isoceles-triangle))
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

(define (circle-line-intersect-test)
  (let-geo* ((cir (random-circle))
             ((rad (a b)) (random-circle-radius cir))
             (p (random-point-on-segment rad))
             (l (random-line-through-point p))
             (cd (intersect-circle-line cir l))
             (c (car cd))
             (d (cadr cd)))
    (figure cir rad p l c d)))

(define (circle-test)
  (let-geo* ((a (random-point))
             (b (random-point))
             (d (distance a b))
             (r (rand-range
                 (* d 0.5)
                 (* d 1)))
             (c1 (make-circle a r))
             (c2 (make-circle b r))
             (cd (intersect-circles c1 c2))
             (c (car cd))
             (d (cadr cd)))
    (figure (polygon-from-points a c b d))))

(define (line-test)
  (let-geo* ((a (random-point))
             (b (random-point))
             (c (random-point))
             (d (random-point))
             (l1 (line-from-points a b))
             (l2 (line-from-points c d))
             (e (intersect-lines l1 l2))
             (f (random-point-on-line l1))
             (cir (circle-from-points e f)))
    (figure a b c d l1 l2 e f cir)))

(define (angle-test)
  (let-geo* (((t (a b c)) (random-triangle))
             (a-1 (smallest-angle (angle-from-points a b c)))
             (a-2 (smallest-angle (angle-from-points b c a)))
             (a-3 (smallest-angle (angle-from-points c a b)))
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
    (figure t cir a-1 a-2 a-3
            pb-center
            circum-cir
            center-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run commands

(define current-figure demo-figure)

(define c
  (if (environment-bound? (the-environment) 'c)
      c
      (canvas)))

(define (close)
  (ignore-errors (lambda () (graphics-close (canvas-g c)))))

(define *num-inner-loop* 5)
(define *num-outer-loop* 5)


(define (run-figure current-figure-proc)
  (let ((analysis-data (make-analysis-collector)))
    (run-animation
     (lambda ()
       (let ((current-figure (current-figure-proc)))
         (draw-figure current-figure c)
         (let ((analysis-results (analyze-figure current-figure)))
           (save-results analysis-results analysis-data))
         )))
    ;(display "--- Results ---\n")
    ;(print-analysis-results analysis-data)
    ))

(define interesting-figures
  (list
   debug-figure
   parallel-lines-converse
   perpendicular-bisector-equidistant
   perpendicular-bisector-converse
   demo-figure
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
