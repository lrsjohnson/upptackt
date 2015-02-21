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
        ((angle? element)
         (draw-angle canvas element))
        ((line? element)
         (draw-line canvas element))
        ((ray? element)
         (draw-ray canvas element))))

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


(define (min-positive . args)
  (min (filter (lambda (x) (>= x 0)) args)))

(define (max-negative . args)
  (min (filter (lambda (x) (<= x 0)) args)))

(define (draw-line canvas line)
  (let ((p1 (line-p1 line))
        (p2 (line-p2 line)))
    (draw-segment canvas (extend-to-max-segment p1 p2))))

(define (draw-ray canvas ray)
  (let ((p1 (ray-p1 ray))
        (p2 (ray-p2 ray)))
    (draw-segment canvas (ray-extend-to-max-segment p1 p2))))

(define (draw-circle canvas c)
  (let ((center (circle-center c))
        (radius (circle-radius c)))
    (canvas-draw-circle canvas
                        (point-x center)
                        (point-y center)
                        radius)))

(define *angle-mark-radius* 0.1)
(define (draw-angle canvas a)
  (let* ((p1 (angle-p1 a))
         (vertex (angle-vertex a))
         (p2 (angle-p2 a))
         (leg1 (sub-points p1 vertex))
         (leg2 (sub-points p2 vertex))
         (angle-start (vec-to-angle leg1))
         (angle-end (vec-to-angle leg2)))
    (canvas-draw-arc canvas
                     (point-x vertex)
                     (point-y vertex)
                     *angle-mark-radius*
                     angle-start
                     angle-end)))

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

(define (canvas-draw-arc canvas x y radius
                         angle-start angle-end)
  (let ((angle-sweep
         (fix-angle-0-2pi (- angle-end
                             angle-start))))
    (graphics-operation (canvas-g canvas)
                        'draw-arc
                        x y radius radius
                        (rad->deg angle-start)
                        (rad->deg angle-sweep)
                        #f)))

(define (canvas-fill-circle canvas x y radius)
  (graphics-operation (canvas-g canvas)
                      'fill-circle
                      x y radius))

(define (canvas-draw-line canvas x1 y1 x2 y2)
  (graphics-draw-line (canvas-g canvas)
                      x1 y1
                      x2 y2))
