;;; thesis-demos.scm -- Examples for thesis demonstration chapter

;;; Code

;;;;;;;;;;;;;;;;;;;;;;;; Basic Figure Example ;;;;;;;;;;;;;;;;;;;;;;;;

(define demo-figure-0
  (let-geo* (((s (a b)) (random-segment))
             (pb (perpendicular-bisector s))
             (p (random-point-on-line pb)))
    (figure s pb
            (make-segment a p)
            (make-segment b p))))

(define demo-figure-1
  (let-geo* ((a (make-point 0 0))
             (b (make-point 1.5 0))
             (c (make-point 1 1))
             (t (polygon-from-points a b c))
             (pb1 (perpendicular-bisector (make-segment a b)))
             (pb2 (perpendicular-bisector (make-segment b c)))
             (pb3 (perpendicular-bisector (make-segment c a))))
    (figure t pb1 pb2 pb3)))

(show-figure
 demo-figure-1)
