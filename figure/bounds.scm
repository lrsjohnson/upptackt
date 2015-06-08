;;; bounds.scm --- Graphics Bounds

;;; Commentary:

;; Ideas:
;; - Logic to extend segments to graphics bounds so they can be drawn.

;; Future:
;; - Separate logical bounds of figures from graphics bounds
;; - Combine logic for line and ray (one vs. two directions)
;; - Should these be a part of "figure" vs. "graphics"
;; - Remapping of entire figures to different canvas dimensions

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Bounds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <bounds>
  (make-bounds x-interval y-interval)
  bounds?
  (x-interval bounds-x-interval)
  (y-interval bounds-y-interval))

(define (bounds-xmin b) (interval-low (bounds-x-interval b)))
(define (bounds-xmax b) (interval-high (bounds-x-interval b)))
(define (bounds-ymin b) (interval-low (bounds-y-interval b)))
(define (bounds-ymax b) (interval-high (bounds-y-interval b)))

(define (print-bounds b)
  `(bounds ,(bounds-xmin b)
           ,(bounds-xmax b)
           ,(bounds-ymin b)
           ,(bounds-ymax b)))
(defhandler print print-bounds bounds?)

;;;;;;;;;;;;;;;;;;;;;;;;;; Bounds Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Max bounds of the graphics window

(define *g-min-x* -2)
(define *g-max-x*  2)
(define *g-min-y* -2)
(define *g-max-y*  2)

;;;;;;;;;;;;;;;; Conversion to segments for Graphics ;;;;;;;;;;;;;;;;;

(define (extend-to-max-segment p1 p2)
  (let ((x1 (point-x p1))
        (y1 (point-y p1))
        (x2 (point-x p2))
        (y2 (point-y p2)))
    (let ((dx (- x2 x1))
          (dy (- y2 y1)))
      (cond
       ((= 0 dx) (make-segment
                  (make-point x1 *g-min-y*)
                  (make-point x1 *g-max-y*)))
       ((= 0 dy) (make-segment
                  (make-point *g-min-x* y1)
                  (make-point *g-min-y* y1)))
       (else
        (let ((t-xmin (/ (- *g-min-x* x1) dx))
              (t-xmax (/ (- *g-max-x* x1) dx))
              (t-ymin (/ (- *g-min-y* y1) dy))
              (t-ymax (/ (- *g-max-y* y1) dy)))
          (let* ((sorted (sort (list t-xmin t-xmax t-ymin t-ymax) <))
                 (min-t (cadr sorted))
                 (max-t (caddr sorted))
                 (min-x (+ x1 (* min-t dx)))
                 (min-y (+ y1 (* min-t dy)))
                 (max-x (+ x1 (* max-t dx)))
                 (max-y (+ y1 (* max-t dy))))
            (make-segment (make-point min-x min-y)
                          (make-point max-x max-y)))))))))

(define (ray-extend-to-max-segment p1 p2)
  (let ((x1 (point-x p1))
        (y1 (point-y p1))
        (x2 (point-x p2))
        (y2 (point-y p2)))
    (let ((dx (- x2 x1))
          (dy (- y2 y1)))
      (cond
       ((= 0 dx) (make-segment
                  (make-point x1 *g-min-y*)
                  (make-point x1 *g-max-y*)))
       ((= 0 dy) (make-segment
                  (make-point *g-min-x* y1)
                  (make-point *g-min-y* y1)))
       (else
        (let ((t-xmin (/ (- *g-min-x* x1) dx))
              (t-xmax (/ (- *g-max-x* x1) dx))
              (t-ymin (/ (- *g-min-y* y1) dy))
              (t-ymax (/ (- *g-max-y* y1) dy)))
          (let* ((sorted (sort (list t-xmin t-xmax t-ymin t-ymax) <))
                 (min-t (cadr sorted))
                 (max-t (caddr sorted))
                 (min-x (+ x1 (* min-t dx)))
                 (min-y (+ y1 (* min-t dy)))
                 (max-x (+ x1 (* max-t dx)))
                 (max-y (+ y1 (* max-t dy))))
            (make-segment p1
                          (make-point max-x max-y)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Rescale Figure ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-bounds (make-bounds (make-interval 0 0)
                                  (make-interval 0 0)))

(define (extend-interval i new-value)
  (let ((low (interval-low i))
        (high (interval-high i)))
    (make-interval (min low new-value)
                   (max high new-value))))

(define (interval-length i)
  (- (interval-high i)
     (interval-low i)))

(define (extend-bounds bounds point)
  (let ((px (point-x point))
        (py (point-y point)))
    (make-bounds
     (extend-interval (bounds-x-interval bounds)
                      px)
     (extend-interval (bounds-y-interval bounds)
                      py))))

(define (bounds-width bounds)
  (interval-length (bounds-x-interval bounds)))

(define (bounds-height bounds)
  (interval-length (bounds-y-interval bounds)))

(define (bounds->square bounds)
  (let ((new-side-length
         (max (bounds-width bounds)
              (bounds-height bounds))))
    (recenter-bounds bounds
                     new-side-length
                     new-side-length)))

(define (recenter-interval i new-length)
  (let* ((min (interval-low i))
         (max (interval-high i))
         (old-half-length (/ (- max min) 2))
         (new-half-length (/ new-length 2)))
    (make-interval (- (+ min old-half-length) new-half-length)
                   (+ (- max old-half-length) new-half-length))))

(define (recenter-bounds bounds new-width new-height)
  (make-bounds
   (recenter-interval (bounds-x-interval bounds) new-width)
   (recenter-interval (bounds-y-interval bounds) new-height)))

(define (scale-bounds bounds scale-factor)
  (recenter-bounds
   bounds
   (* (bounds-width bounds) scale-factor)
   (* (bounds-height bounds) scale-factor)))

(define (extract-bounds figure)
  (let ((all-points (figure-points figure)))
    (let lp ((bounds empty-bounds)
             (points all-points))
      (if (null? points)
          bounds
          (extend-bounds (lp bounds (cdr points))
                         (car points))))))
