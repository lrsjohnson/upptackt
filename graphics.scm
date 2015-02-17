(load "figure.scm")

(define (draw-figure figure-proc canvas)
  (clear-canvas canvas)
  (for-each
   (draw-element canvas)
   (figure-elements (figure-proc))))

(define ((draw-element canvas) element)
  (cond ((point? element)
         (draw-point canvas element))
        ((segment? element)
         (draw-segment canvas element))
        ((circle? element)
         (draw-circle canvas element))
        ((line? element)
         (draw-line canvas element))))

(define *point-radius* 0.02)
(define (draw-point canvas point)
  (canvas-fill-circle canvas
               (point-x point)
               (point-y point)
               *point-radius*))

(define (draw-segment canvas segment)
  (let ((p1 (segment-p1 segment))
        (p2 (segment-p2 segment)))
    (canvas-draw-line canvas
                      (point-x p1)
                      (point-y p1)
                      (point-x p2)
                      (point-y p2))))

(define *g-min-x* -1)
(define *g-max-x*  1)
(define *g-min-y* -1)
(define *g-max-y*  1)

(define (extend-to-max-segment p1 p2)
  (let ((x1 (point-x p1))
        (y1 (point-y p1))
        (x2 (point-x p2))
        (y2 (point-y p2)))
    (let ((dx (- x2 x1))
          (dy (- y2 y1)))
      (cond
       ((= 0 dx) (segment
                  (point x1 *g-min-y*)
                  (point x1 *g-max-y*)))
       ((= 0 dy) (segment
                  (point *g-min-x* y1)
                  (point *g-min-y* y1)))
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
            (segment (point min-x min-y)
                     (point max-x max-y)))))))))

(define (min-positive . args)
  (min (filter (lambda (x) (>= x 0)) args)))

(define (max-negative . args)
  (min (filter (lambda (x) (<= x 0)) args)))

(define (draw-line canvas line)
  (let ((p1 (line-p1 line))
        (p2 (line-p2 line)))
    (draw-segment canvas (extend-to-max-segment p1 p2))))

(define (draw-circle canvas c)
  (let ((center (circle-center c))
        (radius (circle-radius c)))
    (canvas-draw-circle canvas
                        (point-x center)
                        (point-y center)
                        radius)))

;;; Canvas for x-graphics

(define (x-graphics) (make-graphics-device 'x))

(define (canvas)
  (let ((g (x-graphics)))
    (list 'canvas g)))

(define (canvas-g canvas)
  (cadr canvas))

(define canvas? (tag-predicate 'canvas))

(define (clear-canvas canvas)
  (graphics-clear (canvas-g canvas)))

(define (canvas-draw-circle canvas x y radius)
  (graphics-operation (canvas-g canvas)
                      'draw-circle
                      x y radius))

(define (canvas-fill-circle canvas x y radius)
  (graphics-operation (canvas-g canvas)
                      'fill-circle
                      x y radius))

(define (canvas-draw-line canvas x1 y1 x2 y2)
  (graphics-draw-line (canvas-g canvas)
                      x1 y1
                      x2 y2))
