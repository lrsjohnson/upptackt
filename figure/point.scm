;;; point.scm --- Point

;;; Commentary:

;; Ideas:
;; - Points are the basis for most elements

;; Future:
;; - Transform to different canvases
;; - Have points know what elements they are on.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;; Point Structure ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))


(define (print-point p)
    (if (named? p)
        (element-name p)
        `(point ,(point-x p) ,(point-y p))))

(defhandler print
  print-point point?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (point-equal? p1 p2)
  (and (close-enuf? (point-x p1)
                    (point-x p2))
       (close-enuf? (point-y p1)
                    (point-y p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; P2 - P1
(define (sub-points p2 p1)
  (let ((x1 (point-x p1))
        (x2 (point-x p2))
        (y2 (point-y p2))
        (y1 (point-y p1)))
    (make-vec (- x2 x1)
              (- y2 y1))))

;;; Direction from p1 to p2
(define (direction-from-points p1 p2)
  (vec->direction (sub-points p2 p1)))

(define (add-to-point p vec)
  (let ((x (point-x p))
        (y (point-y p))
        (dx (vec-x vec))
        (dy (vec-y vec)))
    (make-point (+ x dx)
                (+ y dy))))
