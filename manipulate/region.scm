;;; regions.scm --- Region Information

;;; Commentary:

;; Ideas:
;; - Points, Lines, Circles, Intersections
;; - For now, semicircle (joints only go to 180deg to avoid
;;     multiple solns.)

;; Future:
;; - Differentiate regions with 2 deg. of freedom
;; - Improve contradiction objects

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Point Sets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <m:point-set>
  (%m:make-point-set points)
  m:point-set?
  (points m:point-set-points))

(define (m:make-point-set points)
  (%m:make-point-set points))

(define (m:in-point-set? p point-set)
  (pair? ((member-procedure point-equal?) p (m:point-set-points point-set))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <m:ray>
  (%m:make-ray endpoint direction)
  m:ray?
  (endpoint m:ray-endpoint)
  (direction m:ray-direction))

(define m:make-ray %m:make-ray)

(define (m:on-ray? p ray)
  (let ((endpoint (m:ray-endpoint ray)))
    (or (point-equal? p endpoint)
        (let ((dir (direction-from-points endpoint p)))
          (direction-equal? dir (m:ray-direction ray))))))

(define (m:p2-on-ray ray)
  (add-to-point (m:ray-endpoint ray)
                (unit-vec-from-direction (m:ray-direction ray))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Semi Circle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <m:arc>
  (m:make-arc center-point radius dir-interval)
  m:arc?
  (center-point m:arc-center)
  (radius m:arc-radius)
  (dir-interval m:arc-dir-interval))

;;; Start direction + ccw pi radian
(define (m:make-semi-circle center radius start-direction)
  (m:make-arc center radius
              (make-direction-interval start-direction
                                       (reverse-direction start-direction))))

(define (m:on-arc? p arc)
  (let ((center-point (m:arc-center arc))
        (radius (m:arc-radius arc)))
    (let ((distance (distance p center-point))
          (dir (direction-from-points center-point p)))
      (and (close-enuf? distance radius)
           (within-direction-interval?
            dir
            (m:arc-dir-interval arc))))))

;;;;;;;;;;;;;;;;;;;;;;; Contradiction Objects ;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <m:region-contradiction>
  (m:make-region-contradiction error-size)
  m:region-contradiction?
  (error-size m:region-contradiction-error))

;;;;;;;;;;;;;;;;;;;;;;; Specific Intersections ;;;;;;;;;;;;;;;;;;;;;;;

(define (m:intersect-rays ray1 ray2)
  (let ((endpoint-1 (m:ray-endpoint ray1))
        (endpoint-2 (m:ray-endpoint ray2))
        (dir-1 (m:ray-direction ray1))
        (dir-2 (m:ray-direction ray2)))
    (if (direction-equal? dir-1 dir-2)
        (cond ((m:on-ray? endpoint-1 ray2) ray1)
              ((m:on-ray? endpoint-2 ray1) ray2)
              ;; TODO: Determine error value
              (else (m:make-region-contradiction 0)))
        (let ((ray1-p2 (m:p2-on-ray ray1))
              (ray2-p2 (m:p2-on-ray ray2)))
          (let ((intersection
                 (intersect-lines-by-points endpoint-1 ray1-p2
                                            endpoint-2 ray2-p2)))
            (if (and (m:on-ray? intersection ray1)
                     (m:on-ray? intersection ray2))
                (m:make-point-set (list intersection))
                ;; TODO: Determine error value
                (m:make-region-contradiction 0)))))))

(define (m:intersect-arcs arc1 arc2)
  (let ((c1 (m:arc-center arc1))
        (c2 (m:arc-center arc2))
        (r1 (m:arc-radius arc1))
        (r2 (m:arc-radius arc2)))
    (if (point-equal? c1 c2)
        (if (close-enuf? r1 r2)
            (m:make-arc c1 r1
                        (intersect-dir-intervals
                         (m:arc-dir-interval arc1)
                         (m:arc-dir-interval arc2)))
            (m:make-region-contradiction 0))
        (let ((intersections
               (intersect-circles-by-centers-radii
                c1 r1 c2 r2)))
          (let ((points
                 (filter (lambda (p)
                           (and (m:on-arc? p arc1)
                                (m:on-arc? p arc2)))
                         intersections)))
            (if (> (length points) 0)
                (m:make-point-set points)
                ;; TODO: Determine error value
                (m:make-region-contradiction 0)))))))

(define (m:intersect-ray-arc ray arc)
  (let ((center (m:arc-center arc))
        (radius (m:arc-radius arc))
        (endpoint (m:ray-endpoint ray))
        (ray-p2 (m:p2-on-ray ray)))
    (let ((intersections
           (intersect-circle-line-by-points
            center radius endpoint ray-p2)))
      (let ((points
             (filter (lambda (p)
                       (and (m:on-ray? p ray)
                            (m:on-arc? p arc)))
                     intersections)))
        (if (> (length points) 0)
            (m:make-point-set points)
            ;; TODO: Determine error value
            (m:make-region-contradiction 0))))))

(define (m:intersect-arc-ray arc ray)
  (m:intersect-ray-arc ray arc))

;;;;;;;;;;;;;;;;;;;; Intersecting with Point Sets ;;;;;;;;;;;;;;;;;;;;

(define m:in-region? (make-generic-operation 2 'm:in-region?))

(defhandler m:in-region? m:in-point-set? point? m:point-set?)
(defhandler m:in-region? m:on-ray? point? m:ray?)
(defhandler m:in-region? m:on-arc? point? m:arc?)
(defhandler m:in-region? (lambda (p r) #f) point? m:region-contradiction?)

(define (m:intersect-point-set-with-region ps1 region)
  (let ((results
         (let lp ((points-1 (m:point-set-points ps1))
                  (point-intersections '()))
           (if (null? points-1)
               point-intersections
               (let ((p1 (car points-1)))
                 (if (m:in-region? p1 region)
                     (lp (cdr points-1)
                         (cons p1 point-intersections))
                     (lp (cdr points-1)
                         point-intersections)))))))
    (if (> (length results) 0)
        (m:make-point-set results)
        ;;; TODO: Determine error value
        (m:make-region-contradiction 0))))

(define (m:intersect-region-with-point-set region ps)
  (m:intersect-point-set-with-region ps region))

;;;;;;;;;;;;;;;;; Generic Intersect Regions "Merge" ;;;;;;;;;;;;;;;;;;

(define m:intersect-regions (make-generic-operation 2 'm:intersect-regions))

;;; Same Type
(defhandler m:intersect-regions
  m:intersect-rays m:ray? m:ray?)
(defhandler m:intersect-regions
  m:intersect-arcs m:arc? m:arc?)

;;; Arc + Ray
(defhandler m:intersect-regions
  m:intersect-ray-arc m:ray? m:arc?)
(defhandler m:intersect-regions
  m:intersect-arc-ray m:arc? m:ray?)

;;; Point Sets
(defhandler m:intersect-regions
  m:intersect-region-with-point-set any? m:point-set?)
(defhandler m:intersect-regions
  m:intersect-point-set-with-region m:point-set? any?)

;;; Contradictions
(defhandler m:intersect-regions (lambda (a b) a) m:region-contradiction? any?)
(defhandler m:intersect-regions (lambda (a b) b) any? m:region-contradiction?)

;;;;;;;;;;;;;;;;;;; Interface to Propagator System ;;;;;;;;;;;;;;;;;;;

(define (m:region? x)
  (or (m:point-set? x)
      (m:ray? x)
      (m:arc? x)
      (m:region-contradiction? x)))

(define m:region-equivalent?
  (make-generic-operation 2 'm:region-equivalent? (lambda (a b) #f)))

(defhandler equivalent? m:region-equivalent? m:region? m:region?)

(defhandler merge m:intersect-regions m:region? m:region?)

(defhandler contradictory? m:region-contradiction? m:region?)

#|
 Simple Examples
 (pp (let-cells (c)
    (add-content c (m:make-arc (make-point 1 0) (sqrt 2)
                               (make-direction-interval
                                (make-direction (/ pi 8))
                                (make-direction (* 7 (/ pi 8))))))

    (add-content c (m:make-ray (make-point -3 1) (make-direction 0)))
    (add-content c (m:make-ray (make-point 1 2)
                 (make-direction (* 7 (/ pi 4)))))
    (content c)))

 (let ((a (make-point 0 0))
       (b (make-point 1 0))
       (c (make-point 0 1))
       (d (make-point 1 1)))
     (let-cells (cell)
       (add-content cell
                    (make-tms
                     (contingent (m:make-point-set (list a b c))
                                 '(a))))
       (add-content cell
                    (make-tms
                     (contingent (m:make-point-set (list a d))
                                 '(a))))
       (pp (tms-query (content cell)))))
|#
