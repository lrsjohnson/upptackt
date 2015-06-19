;;; transforms.scm --- Transforms on Elements

;;; Commentary:

;; Ideas:
;; - Generic transforms - rotation and translation
;; - None mutate points, just return new copies.

;; Future:
;; - Translation or rotation to match something
;; - Consider mutations?
;; - Reflections?

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rotations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Rotates counterclockwise
(define (rotate-point-about rot-origin radians point)
  (let ((v (sub-points point rot-origin)))
    (let ((rotated-v (rotate-vec v radians)))
      (add-to-point rot-origin rotated-v))))

(define (rotate-segment-about rot-origin radians seg)
  (define (rotate-point p) (rotate-point-about rot-origin radians p))
  (make-segment (rotate-point (segment-endpoint-1 seg))
                (rotate-point (segment-endpoint-2 seg))))

(define (rotate-ray-about rot-origin radians r)
  (define (rotate-point p) (rotate-point-about rot-origin radians p))
  (make-ray (rotate-point-about rot-origin radians (ray-endpoint r))
            (add-to-direction (ray-direction r) radians)))

(define (rotate-line-about rot-origin radians l)
  (make-line (rotate-point-about rot-origin radians (line-point l))
             (add-to-direction (line-direction l) radians)))

(define rotate-about (make-generic-operation 3 'rotate-about))
(defhandler rotate-about rotate-point-about point? number? point?)
(defhandler rotate-about rotate-ray-about point? number? ray?)
(defhandler rotate-about rotate-segment-about point? number? segment?)
(defhandler rotate-about rotate-line-about point? number? line?)

(define (rotate-randomly-about p elt)
  (let ((radians (rand-angle-measure)))
    (rotate-about p radians elt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Translations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (translate-point-by vec point)
  (add-to-point point vec))

(define (translate-segment-by vec seg)
  (define (translate-point p) (translate-point-by vec p))
  (make-segment (translate-point (segment-endpoint-1 seg))
                (translate-point (segment-endpoint-2 seg))))

(define (translate-ray-by vec r)
  (make-ray (translate-point-by vec (ray-endpoint r))
            (ray-direction r)))

(define (translate-line-by vec l)
  (make-line (translate-point-by vec (line-point l))
             (line-direction l)))

(define (translate-angle-by vec a)
  (define (translate-point p) (translate-point-by vec p))
  (make-angle (angle-arm-1 a)
              (translate-point (angle-vertex a))
              (angle-arm-2 a)))

(define translate-by (make-generic-operation 2 'rotate-about))
(defhandler translate-by translate-point-by vec? point?)
(defhandler translate-by translate-ray-by vec? ray?)
(defhandler translate-by translate-segment-by vec? segment?)
(defhandler translate-by translate-line-by vec? line?)
(defhandler translate-by translate-angle-by vec? angle?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reflections ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reflect-about-line line p)
  (if (on-line? p line)
      p
      (let ((s (perpendicular-to line p)))
        (let ((v (segment->vec s)))
          (add-to-point
           p
           (scale-vec v 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;; Random Translation ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (translate-randomly-along-line l elt)
  (let* ((vec (unit-vec-from-direction (line->direction l)))
         (scaled-vec (scale-vec vec (rand-range 0.5 1.5))))
    (translate-by vec elt)))

(define (translate-randomly elt)
  (let ((vec (rand-translation-vec-for elt)))
    (translate-by vec elt)))

(define (rand-translation-vec-for-point p1)
  (let ((p2 (random-point)))
    (sub-points p2 p1)))

(define (rand-translation-vec-for-segment seg)
  (rand-translation-vec-for-point (segment-endpoint-1 seg)))

(define (rand-translation-vec-for-ray r )
  (rand-translation-vec-for-point (ray-endpoint r)))

(define (rand-translation-vec-for-line l)
  (rand-translation-vec-for-point (line-point l)))

(define rand-translation-vec-for
  (make-generic-operation 1 'rand-translation-vec-for))
(defhandler rand-translation-vec-for
  rand-translation-vec-for-point point?)
(defhandler rand-translation-vec-for
  rand-translation-vec-for-segment segment?)
(defhandler rand-translation-vec-for
  rand-translation-vec-for-ray ray?)
(defhandler rand-translation-vec-for
  rand-translation-vec-for-line line?)
