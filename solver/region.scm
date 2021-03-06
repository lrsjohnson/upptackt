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

(define (m:make-singular-point-set point)
  (m:make-point-set (list point)))

(define (m:in-point-set? p point-set)
  (pair? ((member-procedure point-equal?) p (m:point-set-points point-set))))

(define (m:singular-point-set? x)
  (and (m:point-set? x)
       (= 1 (length (m:point-set-points x)))))

(define (m:singular-point-set-point ps)
  (if (not (m:singular-point-set? ps))
      (error "Not a singular point set"))
  (car (m:point-set-points ps)))

(define (m:point-sets-equivalent? ps1 ps2)
  (define delp (delete-member-procedure list-deletor point-equal?))
  (define memp (member-procedure point-equal?))
  (let lp ((points-1 (m:point-set-points ps1))
           (points-2 (m:point-set-points ps2)))
    (if (null? points-1)
        (null? points-2)
        (let ((p1 (car points-1)))
          (if (memp p1 points-2)
              (lp (cdr points-1)
                  (delp p1 points-2))
              #f)))))

(define (m:print-point-set ps)
  (cons 'm:point-set
        (map (lambda (p) (list 'point (point-x p) (point-y p)))
             (m:point-set-points ps))))

(defhandler print
  m:print-point-set m:point-set?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <m:ray>
  (%m:make-ray endpoint direction)
  m:ray?
  (endpoint m:ray-endpoint)
  (direction m:ray-direction))

(define m:make-ray %m:make-ray)

(define (m:ray->figure-ray m-ray)
  (with-color "red"
              (make-ray (m:ray-endpoint m-ray)
                        (m:ray-direction m-ray))))

(define (m:on-ray? p ray)
  (let ((endpoint (m:ray-endpoint ray)))
    (or (point-equal? p endpoint)
        (let ((dir (direction-from-points endpoint p)))
          (direction-equal? dir (m:ray-direction ray))))))

(define (m:p2-on-ray ray)
  (add-to-point (m:ray-endpoint ray)
                (unit-vec-from-direction (m:ray-direction ray))))

(define (m:rays-equivalent? ray1 ray2)
  (and (point-equal? (m:ray-endpoint ray1)
                     (m:ray-endpoint ray2))
       (direction-equal? (m:ray-direction ray1)
                         (m:ray-direction ray2))))

(define (m:print-ray ray)
  (let ((endpoint (m:ray-endpoint ray)))
    `(m:ray (,(point-x endpoint)
             ,(point-y endpoint))
            ,(direction-theta (m:ray-direction ray)))))

(defhandler print
  m:print-ray m:ray?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Arcs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (m:arcs-equivalent? arc1 arc2)
  (and (point-equal? (m:arc-center arc1)
                     (m:arc-center arc2))
       (close-enuf? (m:arc-radius arc1)
                    (m:arc-radius arc2))
       (direction-interval-equal?
        (m:arc-dir-interval arc1)
        (m:arc-dir-interval arc2))))

(define (m:print-arc arc)
  (let ((center-point (m:arc-center arc))
        (dir-interval (m:arc-dir-interval arc)))
    `(m:arc (,(point-x center-point)
             ,(point-y center-point))
            ,(m:arc-radius arc)
            (,(direction-theta (direction-interval-start dir-interval))
             ,(direction-theta (direction-interval-end dir-interval))))))

(defhandler print
  m:print-arc
  m:arc?)

;;;;;;;;;;;;;;;;;;;;;;; Contradiction Objects ;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <m:region-contradiction>
  (m:make-region-contradiction error-regions)
  m:region-contradiction?
  (error-regions m:contradiction-error-regions))

;;;  Maybe differeniate by error values?
(define (m:region-contradictions-equivalent? rc1 rc2) #t)

(define (m:region-contradiction->figure-elements rc)
  (map m:region->figure-elements (m:contradiction-error-regions rc)))

;;;;;;;;;;;;;;;;;;;;;;; Specific Intersections ;;;;;;;;;;;;;;;;;;;;;;;

(define (m:intersect-rays ray1 ray2)
  (let ((endpoint-1 (m:ray-endpoint ray1))
        (endpoint-2 (m:ray-endpoint ray2))
        (dir-1 (m:ray-direction ray1))
        (dir-2 (m:ray-direction ray2)))
    (if (direction-equal? dir-1 dir-2)
        (cond ((m:on-ray? endpoint-1 ray2) ray1)
              ((m:on-ray? endpoint-2 ray1) ray2)
              (else (m:make-region-contradiction (list ray1 ray2))))
        (let ((ray1-p2 (m:p2-on-ray ray1))
              (ray2-p2 (m:p2-on-ray ray2)))
          (let ((intersections
                 (intersect-lines-by-points endpoint-1 ray1-p2
                                            endpoint-2 ray2-p2)))
            (if (not (= 1 (length intersections)))
                (m:make-region-contradiction (list ray1 ray2))
                (let ((intersection (car intersections)))
                 (if (and (m:on-ray? intersection ray1)
                          (m:on-ray? intersection ray2))
                     (m:make-point-set (list intersection))
                     (m:make-region-contradiction (list ray1 ray2))))))))))

(define (m:intersect-arcs arc1 arc2)
  (let ((c1 (m:arc-center arc1))
        (c2 (m:arc-center arc2))
        (r1 (m:arc-radius arc1))
        (r2 (m:arc-radius arc2)))
    (if (point-equal? c1 c2)
        (if (close-enuf? r1 r2)
            (m:make-arc c1 r1
                        (intersect-direction-intervals
                         (m:arc-dir-interval arc1)
                         (m:arc-dir-interval arc2)))
            (m:make-region-contradiction (list arc1 arc2)))
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
                (m:make-region-contradiction (list arc1 arc2))))))))

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
            (m:make-region-contradiction (list ray arc)))))))

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
        (m:make-region-contradiction (list ps1 region)))))

(define (m:intersect-region-with-point-set region ps)
  (m:intersect-point-set-with-region ps region))

;;;;;;;;;;;;;;;;;;;;; Translating regions by Vec ;;;;;;;;;;;;;;;;;;;;;

(define m:translate-region (make-generic-operation 2 'm:translate-region))

(define (m:translate-point-set ps vec)
  (m:make-point-set
   (map (lambda (p) (add-to-point p vec))
        (m:point-set-points ps))))
(defhandler m:translate-region m:translate-point-set m:point-set? vec?)

(define (m:translate-ray ray vec)
  (m:make-ray
   (add-to-point (m:ray-endpoint ray) vec)
   (m:ray-direction ray)))
(defhandler m:translate-region m:translate-ray m:ray? vec?)

(define (m:translate-arc arc vec)
  (m:make-arc
   (add-to-point (m:arc-center arc) vec)
   (m:arc-radius arc)
   (m:arc-dir-interval arc)))
(defhandler m:translate-region m:translate-arc m:arc? vec?)

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

;;;;;;;;;;;;;;;;;;;;;;;; Generic Equivalency ;;;;;;;;;;;;;;;;;;;;;;;;;

(define m:region-equivalent?
  (make-generic-operation 2 'm:region-equivalent? (lambda (a b) #f)))

(defhandler m:region-equivalent?
  m:point-sets-equivalent? m:point-set? m:point-set?)

(defhandler m:region-equivalent?
  m:rays-equivalent? m:ray? m:ray?)

(defhandler m:region-equivalent?
  m:arcs-equivalent? m:arc? m:arc?)

(defhandler m:region-equivalent?
  m:region-contradictions-equivalent?
  m:region-contradiction?
  m:region-contradiction?)

;;;;;;;;;;;;;;;;;;; Interface to Propagator System ;;;;;;;;;;;;;;;;;;;

(define (m:region? x)
  (or (m:point-set? x)
      (m:ray? x)
      (m:arc? x)
      (m:region-contradiction? x)))


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
;;;;;;;;;;;;;;;;;;;;;;;;; To Figure elements ;;;;;;;;;;;;;;;;;;;;;;;;;

(define m:region->figure-elements
  (make-generic-operation 1 'm:region->figure-elements (lambda (r) #f )))

(defhandler m:region->figure-elements
  m:ray->figure-ray
  m:ray?)

(defhandler m:region->figure-elements
  m:region-contradiction->figure-elements
  m:region-contradiction?)
