;;; region-info.scm --- Region Information

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

(define (m:make-point-set . points)
  (%m:make-point-set points))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <m:ray>
  (%m:make-ray vertex direction)
  m:ray?
  (vertex m:ray-vertex)
  (direction m:ray-direction))

(define (m:on-ray? p ray)
  (let ((vertex (m:ray-vertex ray)))
    (let ((dir (sub-points p vertex)))
      (direction-equal? dir (m:ray-direction ray)))))

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
          (dir (sub-points p center-point)))
      (and (close-enuf? distance)
           (within-direction-interval?
            dir
            (m:arc-dir-interval arc))))))

;;;;;;;;;;;;;;;;;;;;;;; Contradiction Objects ;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <m:region-contradiction>
  (m:make-region-contradiction error-size)
  m:region-contradiction?
  (error-size m:region-contradiction-error))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Intersections ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:intersect-point-sets ps1 ps2)
  (define delp (delete-member-procedure list-deletor point-equal?))
  (define memp (member-procedure point-equal?))
  (let ((result
         (let lp ((points-1 (m:point-set-points ps1))
                  (points-2 (m:point-set-points ps2))
                  (point-intersections '()))
           (if (null? points-1)
               point-intersections
               (let ((p1 (car points-1)))
                 (if (memp p1 points-2)
                     (lp (cdr points-1)
                         (delp p1 points-2)
                         (cons p1 point-intersections))
                     (lp (cdr points-1)
                         points-2
                         point-intersections)))))))
    ;; TODO: Process contradiction object better.
    (apply m:make-point-set result)))

(define m:intersect-regions (make-generic-operation 2 'm:intersect-regions))

(defhandler m:intersect-regions
  m:intersect-point-sets m:point-set? m:point-set?)
