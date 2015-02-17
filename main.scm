(load "figure.scm")
(load "graphics.scm")

(define (demo-figure)
  (let* ((a (random-point))
         (b (random-point))
         (c (random-point))
         (d (midpoint a b))
         (e (midpoint a c))
         (f (midpoint b c))
         (l1 (line d c))
         (l2 (line e b))
         (l3 (line f a))
         (g (intersect-lines l1 l2)))
    (figure a b c d e f l1 l2 l3 g
            (segment a b)
            (segment a c)
            (segment b c)
            )))

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
    (figure a b c1 c2 c d
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
         (e (intersect-lines l1 l2)))
    (figure a b c d l1 l2 e)))

(define c (canvas))

(draw-figure circle-test c)

;;;

'(triangle a b c)
'(midpoint (a b) c)
'(perp-bisector (a b) c)
