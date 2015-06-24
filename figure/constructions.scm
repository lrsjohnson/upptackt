;;; constructions.scm --- Constructions

;;; Commentary:

;; Ideas:
;; - Various logical constructions that can be peformed on elements
;; - Some higher-level constructions...

;; Future:
;; - More constructions?
;; - Separation between compass/straightedge and compound?
;; - Experiment with higher-level vs. learned constructions

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;; Segment Constructions ;;;;;;;;;;;;;;;;;;;;;;;;

(define (midpoint p1 p2)
  (let ((newpoint
         (make-point (avg (point-x p1)
                          (point-x p2))
                     (avg (point-y p1)
                          (point-y p2)))))
    (save-obvious-observation!
     (make-observation equal-length-relationship
                       (list
                        (make-segment p1 newpoint)
                        (make-segment p2 newpoint))))
    newpoint))

(define (segment-midpoint s)
  (let ((p1 (segment-endpoint-1 s))
        (p2 (segment-endpoint-2 s)))
    (midpoint p1 p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (on-segment? p seg)
  (let ((seg-start (segment-endpoint-1 seg))
        (seg-end (segment-endpoint-2 seg)))
    (or (point-equal? seg-start p)
        (point-equal? seg-end p)
        (let ((seg-length (distance seg-start seg-end))
              (p-length (distance seg-start p))
              (dir-1 (direction-from-points seg-start p))
              (dir-2 (direction-from-points seg-start seg-end)))
          (and (direction-equal? dir-1 dir-2)
               (< p-length seg-length))))))

(define (on-line? p l)
  (let ((line-pt (line-point l))
        (line-dir (line-direction l)))
    (or (point-equal? p line-pt)
        (let ((dir-to-p (direction-from-points p line-pt)))
          (or (direction-equal? line-dir dir-to-p)
              (direction-equal? line-dir (reverse-direction dir-to-p)))))))

(define (on-ray? p r)
  (let ((ray-endpt (ray-endpoint r))
        (ray-dir (ray-direction r)))
    (or (point-equal? ray-endpt p)
        (let ((dir-to-p (direction-from-points ray-endpt p)))
          (direction-equal? dir-to-p ray-dir)))))

;;;;;;;;;;;;;;;;;;;;;;; Construction of lines ;;;;;;;;;;;;;;;;;;;;;;;;

(define (perpendicular linear-element point)
  (let* ((direction (->direction linear-element))
         (rotated-direction (rotate-direction-90 direction))
         (new-line (make-line point rotated-direction)))
    (save-obvious-observation!
     (make-observation
      perpendicular-relationship
      (list linear-element new-line)))
    new-line))

;;; endpoint-1 is point, endpoint-2 is on linear-element
(define (perpendicular-to linear-element point)
  (let ((pl (perpendicular linear-element point)))
    (let ((i (intersect-linear-elements pl (->line linear-element))))
      (let ((seg (make-segment point i)))
        (save-obvious-observation!
         (make-observation
          perpendicular-relationship
          (list seg linear-element)))
        seg))))

(define (perpendicular-line-to linear-element point)
  (let ((pl (perpendicular linear-element point)))
    pl))

(define (perpendicular-bisector segment)
  (let ((midpt (segment-midpoint segment)))
    (let ((pb (perpendicular (segment->line segment)
                             midpt)))
      (save-obvious-observation!
       (make-observation perpendicular-relationship
                         (list segment pb)))
      pb)))

(define (angle-bisector a)
  (let* ((d1 (angle-arm-1 a))
         (d2 (angle-arm-2 a))
         (vertex (angle-vertex a))
         (radians (angle-measure a))
         (half-angle (/ radians 2))
         (new-direction (add-to-direction d2 half-angle)))
    (save-obvious-observation!
     (make-observation
      equal-angle-relationship
      (list (make-angle d2 vertex new-direction)
            (make-angle new-direction vertex d1))))
    (make-ray vertex new-direction)))

(define (polygon-angle-bisector polygon vertex-angle)
  (angle-bisector (polygon-angle polygon vertex-angle)))

;;;;;;;;;;;;;;;;;;;;; Higher-order constructions ;;;;;;;;;;;;;;;;;;;;;

(define (circumcenter t)
  (let ((p1 (polygon-point-ref t 0))
        (p2 (polygon-point-ref t 1))
        (p3 (polygon-point-ref t 2)))
    (let ((l1 (perpendicular-bisector (make-segment p1 p2)))
          (l2 (perpendicular-bisector (make-segment p1 p3))))
      (intersect-linear-elements l1 l2))))

;;;;;;;;;;;;;;;;;;;;; Concurrent Linear Elements ;;;;;;;;;;;;;;;;;;;;;

(define (concurrent? l1 l2 l3)
  (let ((i-point (intersect-linear-elements-no-endpoints l1 l2)))
    (and i-point
         (on-element? i-point l3)
         (not (element-endpoint? i-point l3)))))

(define (concentric? p1 p2 p3 p4)
  (and (distinct? (list p1 p2 p3 p4) point-equal?)
       (let ((pb-1 (perpendicular-bisector
                    (make-segment p1 p2)))
             (pb-2 (perpendicular-bisector
                    (make-segment p2 p3)))
             (pb-3 (perpendicular-bisector
                    (make-segment p3 p4))))
         (concurrent? pb-1 pb-2 pb-3))))

(define (collinear? p1 p2 p3)
  (and (distinct? (list p1 p2 p3) point-equal?)
       (on-line? p3 (line-from-points p1 p2))))

(define (concentric-with-center? center p1 p2 p3)
  (let ((d1 (distance center p1))
        (d2 (distance center p2))
        (d3 (distance center p3)))
    (and (close-enuf? d1 d2)
         (close-enuf? d1 d3))))
