
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
;;; TODO: Translate randomly *multiple*
;;; TODO: Multiple return values

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
;;; TODO: Analyze equal segments not actually there...

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
;;; TODO: aux-segment

;;; [7] Shortest distance conjecture
;;; Givens: arbitrary point p, point a on line l
;;; Goal: Discover that shortest distance to line is along perpendicular
(define (shortest-distance)
  (let-geo* ((p (random-point))
             (l (random-line))
             (a (random-point-on-line l)))
    (figure p l a (make-auxiliary-segment p a))))
;;; TODO: Tricky, figure out how to minimize value, specify "minimize" property?

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
;;; TODO: Concurrency of lines
;;; TODO: Draw markings for angle bisector

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
;;; TODO: Resist redundant concurrencies
;;; TODO: See if it can provide/learn a name for this point?

;;;  [12] Circumcenter Conjecture
(define (circumcenter-figure)
  (let-geo* (((t (a b c)) (random-triangle))
             (c-center (circumcenter t)))
    (figure t c-center (circle-from-points c-center a))))
;;; TODO: Circumcenter macro?

;;; [13] Incenter Conjecture
;;; [14] Median Concurrency Conjecture
;;; [15] Centroid Ratio Conjecture
;;; [16] Center of Gravity Conjecture
;;; [Exp.1] Euler Line Conjecture
;;; [Exp.2] Euler Segment Conjecture
;;; [17] Triangle Sum Conjecture
;;; [18] Isoceles Triangle Conjecture
;;; [19] Converse of Isoceles Triangle Conjecture
;;; [20] Triangle Inequality Conjecture
;;; [21] Side-Angle Inequaity Conjecture
;;; [22] Triangle Exterior Angle Conjecture
;;; [23] SSS Congruence Conjecture
;;; [24] SAS Congruence Conjecture
;;; [24b] SSA - Congruencey?
;;; TODO: Provide some property to consider truth
;;; [25] ASA Congruence Conjecture
;;; [26] SAA Congruence Conjecture
;;; [26b] AAA - Congruency?
;;; [27] Vertex Angle Bisector Conjecture
;;; [28] Equilateral/Eqiangular Triangle Conjecture
;;; [29] Quadrilateral Sum Conjecture
;;; [30] Pentagon Sum Conjecture
;;; [31] Polygon Sum Conjecture
;;; [32] Exterior Angle Sum Conjecture
;;; [33] Equiangular Polygon Conjecture
;;; [34] Kite Angles Conjecture
;;; [35] Kite Diagonals Conjecture
;;; [36] Kite Diagonal Biesctor Conjecture
;;; [37] Kite Angle Bisector Conjecture
;;; [38] Trapezoid Consecutive Angles Conjecture
;;; [39] Isoceles Trapezoid Conjecture
;;; [40] Isoceles Trapezoid Diagonals Conjecture
;;; [41] Three Midsegments Conjecture
;;; [42] Triangle Midsegment Conjecture
;;; [43] Trapezoid Midsegment Conjecture
;;; [44] Parallelogram Opposite Angles Conjecture

(define (parallelogram-opposite-angles)
  (let-geo*
      (((p (a b c d)) (random-parallelogram)))
    (figure p)))
#|
;;; [45] Parallelogram Consecutive Angles Conjecture
;;; [46] Parallelogram Opposite Sides Conjecture
;;; [47] Parallelogram Diagonals Conjecture
;;; [48] Double-Edged Straitedge Conjecture
;;; [49] Thombus Diagonals Conjecture
;;; [50] Rhombus Angles Conjecture
;;; [51] Rectangle Diagonals Conjecture
;;; [52] Square Diagonals Conjecture
;;; [53] Tangent Conjecture
;;; [54] Tangent Segment Conjecture
;;; [55] Chord Central Angles Conjecture
;;; [56] Chord Arcs Conjecture
;;; [57] Perpendicular to a Chord Conjecture
;;; [58] Chord Distance to Center Conjecture
;;; [59] Perpendicular Bisector of a Chord Conjecture
;;; [60] Inscribed Angle Conjecture
;;; [61] Inscribed Angles Intercepting Arcs Conjecture
;;; [62] Angles Inscribed in a Semicircle Conjecture
;;; [63] Cyclic Quadrilateral Conjecture
|#
(define (cyclic-quadrilateral)
  (let-geo*
   ((cir (random-circle))
    (((a b c d)) (n-random-points-on-circle-ccw cir 4))
    (q (polygon-from-points a b c d)))
   (figure q)))
#|
;;; [64] Parallel Lines Intercepted Arcs Conjecture
;;; [65] Circumference Conjecture
;;; [66] Arc Length Conjecture
;;; [Exp.3] Intersecting Seacants Conjecture
;;; [Exp.4] Intersecting Chords Conjecture
;;; [Exp.5] Tangent-Secant Conjecture
;;; [Exp.6] Intersecting Tangents Conjecture
;;; [Exp.7] Tangent-Chord Conjecture
;;; [67] Reflection Line Conjecture
;;; [68] Coordinate Transforms Conjecture
;;; [69] Minimal Path Conjecture
;;; [70] Reflections Across Parallel Lines Conjecture
;;; [71] Reflections Across Intersecting Lines Conjecture
;;; [72] Tessellating Triangles Conjecture
;;; [73] Tesselating Quadrilateral Conjecture
;;; [74] Rectangle Area Conjecture
;;; [75] Parallelogram Area Conjecture
;;; [76] Triangle Area Conjecture
;;; [77] Trapezoid Area Conjecture
;;; [78] Kite Area Conjecture
;;; [79] Regular Polygon Area Conjecture
;;; [80] Circle Area Conjecture
;;; [81] Pythagorean Theorem
;;; [82] Converse of Pythagorean Theorem
;;; [83] Isoceles Right Triangle Conjecture
;;; [84] 30-60-90 Triangle Conjecture
;;; [85] Distance Formula
;;; [86] Prism-Cylinder Volume Conjecture
;;; [87] Pyramid-Cone Volume Conjecture
;;; [Exp.8] Platonic Solids
;;; [88] Sphere Volume Conjecture
;;; [89] Sphere Surface Area Conjecture
;;; [91] AA Similarity Conjecture
;;; [92] SSS Similarity Conjecture
;;; [93] SAS Similarity Conjecture
;;; [94] Proportional Parts Conjecture
;;; [95] Angle Bisector / Opposite Side Conjecture
;;; [96] Proportional Area Conjecture
;;; [97] Proportional Volumes Conjecture
;;; [98] Parallel/Porportionality Conjecture
;;; [99] Extended Parallel/Proportionality Conjecture
;;; [100] SAS Triangle Area Conjecture
;;; [101] Las of Sines
;;; [102] Law of Cosines
;;; [Exp.9] Special Constructions
|#