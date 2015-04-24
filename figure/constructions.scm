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
    (with-dependency
     `(midpoint ,(element-dependency p1) ,(element-dependency p2))
     newpoint)))

(define (segment-midpoint s)
  (let ((p1 (segment-endpoint-1 s))
        (p2 (segment-endpoint-2 s)))
    (with-dependency
     `(segment-midpoint ,(element-dependency s))
     (midpoint p1 p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: Where to put these?
(define (on-segment? p seg)
  (let ((p1 (segment-endpoint-1 seg))
        (p2 (segment-endpoint-2 seg)))
    (let ((d1 (distance p p1))
          (d2 (distance p p2))
          (d3 (distance p1 p2)))
      (close-enuf? (+ d1 d2) d3))))

;;; TODO: Fix for lines / segments
(define (on-line? p l)
  (on-segment?
   p
   (make-segment (line-p1 l)
                 (line-p2 l))))

;;;;;;;;;;;;;;;;;;;;;;; Construction of lines ;;;;;;;;;;;;;;;;;;;;;;;;

(define (perpendicular linear-element point)
  (let* ((direction (->direction linear-element))
         (rotated-direction (rotate-direction-90 direction)))
    (make-line point rotated-direction)))

(define (perpendicular-bisector segment)
  (let ((midpt (segment-midpoint segment)))
    (perpendicular (segment->line segment)
                   midpt)))

(define (angle-bisector a)
  (let* ((d1 (angle-arm-1 a))
         (d2 (angle-arm-2 a))
         (vertex (angle-vertex a))
         (radians (angle-measure a))
         (half-angle (/ radians 2))
         (new-direction (add-to-direction d2 half-angle)))
    (make-ray vertex new-direction)))
