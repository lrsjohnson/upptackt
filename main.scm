
(define (demo-figure)
  (let* ((a (random-point))
         (b (random-point))
         (c (random-point))
         (d (midpoint a b))
         (e (midpoint a c))
         (f (midpoint b c))
         (l1 (perpendicular (line a b) d))
         (l2 (perpendicular (line a c) e))
         (l3 (perpendicular (line b c) f))
         (i (intersect-lines l1 l2))
         (cir (circle-from-points i a)))
    (figure a b c l1 l2 l3 i cir
            (segment a b)
            (segment a c)
            (segment b c))))

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
            (segment a c)
            (segment a d)
            (segment b c)
            (segment b d))))

(define (line-test)
  (let* ((a (random-point))
         (b (random-point))
         (c (random-point))
         (d (random-point))
         (l1 (line a b))
         (l2 (line c d))
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
          (perpendicular (line b c)
                         center-point))
         (radius-point
          (intersect-lines radius-line
                           (line b c)))
         (cir (circle-from-points
               center-point
               radius-point))
         (pb1 (perpendicular-bisector
               (segment a b)))
         (pb2 (perpendicular-bisector
               (segment b c)))
         (pb-center (intersect-lines pb1 pb2))
         (circum-cir (circle-from-points
                      pb-center
                      a)))
    (figure a b c cir
            pb-center
            circum-cir
            center-point
            (segment a b)
            (segment a c)
            (segment b c))))


(define c '())

(define (r)
  (if (null? c)
      (set! c (canvas)))
  (draw-figure angle-test c))

(r)
