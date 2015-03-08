(define (debug-figure)
  (let-geo* ((a (random-point))
             (l (line-through-point a))
             (r (ray-from-point a)))
            (figure a l r)))

(define (demo-figure)
  (let-geo* ((a (random-point))
             (b (random-point))
             (c (random-point))
             (d (midpoint a b))
             (e (midpoint a c))
             (f (midpoint b c))
             (l1 (perpendicular (line a b) d))
             (l2 (perpendicular (line a c) e))
             (l3 (perpendicular (line b c) f))
             (i1 (intersect-lines l1 l2))
             (i2 (intersect-lines l1 l3))
             (cir (circle-from-points i1 a)))
            (figure
             (segment a b)
             (segment b c)
             (segment a c)
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
    (figure a b c cir a-1 a-2 a-3
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
  (draw-figure vertical-angles c))

(define (a)
  (analyze-figure vertical-angles))

(r)

(a)
