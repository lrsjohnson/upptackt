;;; Data structure for a polygon, implemented as a list of
;;; points in counter-clockwise order.
;;; Drawing a polygon will draw all of its points and segments.
(define-record-type <polygon>
  (%polygon n-points points)
  polygon?
  (n-points polygon-n-points)
  (points polygon-points))

(define (polygon-from-points . points)
  (let ((n-points (length points)))
    (%polygon n-points points)))

(define (polygon-point-ref polygon i)
  (if (>= i (polygon-n-points polygon))
      (error "polygon-point-ref not in range"))
  (list-ref (polygon-points polygon) i))
;;; TODO: Dependencies of points obtained from polygon.

(define (polygon-segments polygon)
  (let ((n-points (polygon-n-points polygon)))
    (map (lambda (pair)
           (make-segment (polygon-point-ref polygon (car pair))
                         (polygon-point-ref polygon (cadr pair))))
         (zip
          (iota n-points)
          (map (lambda (i) (modulo (+ i 1) n-points))
               (iota n-points))))))
