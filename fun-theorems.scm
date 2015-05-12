;;; Inspired by examples in Automated Generation of Geometric Theorems
;;; from Images of Diagrams - Xiaoyu Chen Dan Song and Dongming Wangb

;;; [1] Linear Pair Conjecture
;;; Givens: Angles a-1 and a-2 form a linear pair
;;; Goal: m(a-1) + m(a-2) = 180 degrees
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
