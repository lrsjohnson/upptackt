;;; Inspired by examples in Automated Generation of Geometric Theorems
;;; from Images of Diagrams - Xiaoyu Chen Dan Song and Dongming Wangb

(define (quad-midpoint-extension)
  (let-geo* (((q (a b c d)) (random-quadrilateral))
             (e (midpoint a b))
             (f (midpoint c d))
             (r1 (ray-from-points d b))
             (r2 (ray-from-points c a))
             (g (random-point-on-ray
                 (shorten-ray-from-point r2 a)))
             (lg1 (line-from-points g e))
             (lg2 (line-from-points g f))
             (i (intersect-linear-elements
                 lg1 (make-segment b c)))
             (j (intersect-linear-elements
                 lg2 (make-segment a d)))
             (lh1 (line-from-points f i))
             (lh2 (line-from-points e j)))
    (figure q e f r1 r2 g lg1 lg2 i j lh1 lh2)))

(define (two-segments-intersections)
  (let-geo* (((q (a b c d)) (random-quadrilateral))
             (e (random-point-on-segment (make-segment a b)))
             (f (random-point-on-segment (make-segment c d))))
    (figure q e f
            (make-segment a c)
            (make-segment b d)
            (make-segment a f)
            (make-segment b f)
            (make-segment c e)
            (make-segment d e))))
