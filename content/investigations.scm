;;; investigations.scm -- Some sample investigations and ideas that
;;; could be persued

;;; [1] Linear Pair Conjecture
;;; Givens: Angles a-1 and a-2 form a linear pair
;;; Goal: m(a-1) + m(a-2) = 180 degrees
(define (linear-pair)
  (let-geo* ((a (random-point))
             (l1 (random-line-through-point a))
             (r (random-ray-from-point a))
             (a-1 (smallest-angle-from l1 r))
             (a-2 (smallest-angle-from r (flip l1))))
    (figure a l1 r a-1 a-2)))

;;; [2] Vertical Angles Conjecture
;;; Givens: Angles a-1 and a-2 are vertical angles
;;; Goal: m(a-1) = m(a-2)
(define (vertical-angles)
  (let-geo* ((l1 (random-line))
             (c (random-point-on-line l1))
             (l2 (rotate-randomly-about c l1))
             (a-1 (smallest-angle-from l1 l2))
             (a-2 (smallest-angle-from (flip l1) (flip l2))))
    (figure l1 c l2 a-1 a-2)))

;;; [3a] Corresponding Angles Conjecture
;;; Givens: - Lines l1 and l2 are parallel
;;;         - Line l3 is a transversal
;;;         - a-1 and a-2 are resulting corresponding angles
;;; Goal: m(a-1) = m(a-2)
(define (corresponding-angles)
  (let-geo* ((l1 (random-line))
             (l2 (translate-randomly l1))
             (a (random-point-on-line l1))
             (b (random-point-on-line l2))
             (l3 (line-from-points a b))
             (a-1 (smallest-angle-from l3 l2))
             (a-2 (smallest-angle-from l3 l1)))
            (figure l1 l2 a b l3 a-1 a-2)))

;;; [3b, 3c] Interior / alternate interior: ordering of angles and

;;; [4] Converse of Parallel lines
;;; Givens: - m(a-1) = m(a-2)
;;;         - a-1, a-2, are either CA, AIA, AEA, etc. of Lines l1, l2
;;; Goal: lines l1 and l2 are parallel
(define (parallel-lines-converse)
  (let-geo* ((a-1 (random-angle))
             (l3 (line-from-arm-1 a-1))
             (a-2 (translate-randomly-along-line l3 a-1))
             (l1 (line-from-arm-2 a-1))
             (l2 (line-from-arm-2 a-2)))
    (figure a-1 a-2 l1 l2 l3)))

;;; [5] Perpendicular bisector conjecture
;;; Givens: - p is a point on perpendicular bisector of segment (a, b)
;;; Goal: p is equidistant from a and b
(define (perpendicular-bisector-equidistant)
  (let-geo* (((s (a b)) (random-segment))
             (l1 (perpendicular-bisector s))
             (p (random-point-on-line l1)))
            (figure s l1 p)))

;;; [6] Converse of perpendicular bisector conjecture
;;; Given: - a and b are equidistant from point p
;;; Goal: p is on the perpendicular bisector of a, b
(define (perpendicular-bisector-converse)
  (let-geo* ((p (random-point))
             (a (random-point))
             (b (rotate-randomly-about p a))
             (s (make-segment a b))
             (pb (perpendicular-bisector s)))
            (figure p a b s pb)))

;;; [8] Angle bisector conjecture
;;; Given: angle a-1 of rays r-1, r-2, point a on angle-bisector l1
;;; Goal: Distnace from a to r-1 = distance a to r-2

(define (angle-bisector-distance)
  (let-geo* (((a (r-1 v r-2)) (random-angle))
             (ab (angle-bisector a))
             (p (random-point-on-ray ab))
             ((s-1 (p b)) (perpendicular-to r-1 p))
             ((s-2 (p c)) (perpendicular-to r-2 p)))
     (figure a r-1 r-2 ab p s-1 s-2)))
;;; Interesting, dependent on "shortest distance" from prior conjecture

;;; [9] Angle bisector concurrency
;;; Given: Triangle abc with angle-bisectors l1, l2, l3
;;; Goal: l1, l2, l3 are concurrent
(define (angle-bisector-concurrency)
  (let-geo* (((t1 (a b c)) (random-triangle))
             (((a-1 a-2 a-3)) (polygon-angles t1))
             (l1 (polygon-angle-bisector t1 a))
             (l2 (polygon-angle-bisector t1 b))
             (l3 (polygon-angle-bisector t1 c)))
    (figure t1 l1 l2 l3)))

;;; [10] Perpendicular Bisector Concurrency
;;; Given: Triangle ABC with sides s1, s2, s3, perpendicular bisectors
;;; l1, l2, l3
;;; Goal: l1, l2, l3 are concurrent
(define (perpendicular-bisector-concurrency)
  (let-geo* (((t (a b c)) (random-triangle))
             (l1 (perpendicular-bisector (make-segment a b)))
             (l2 (perpendicular-bisector (make-segment b c)))
             (l3 (perpendicular-bisector (make-segment c a))))
    (figure t l1 l2 l3)))

;;; [11] Altitude Concurrency
;;; Given: Triangle ABC with altituds alt-1, alt2, alt-3
;;; Goal: alt-1, alt-2, alt-3 are concurrent
(define (altitude-concurrency)
  (let-geo* (((t (a b c)) (random-triangle))
             (alt-1 (perpendicular-line-to (make-segment b c) a))
             (alt-2 (perpendicular-line-to (make-segment a c) b))
             (alt-3 (perpendicular-line-to (make-segment a b) c)))
            (figure t alt-1 alt-2 alt-3)))
