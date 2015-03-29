(define (draw-figure figure canvas)
  (clear-canvas canvas)
  (for-each
   (lambda (element)
     ((draw-element element) canvas))
   (figure-elements figure))
  (graphics-flush (canvas-g canvas))
  )

(define draw-element
  (make-generic-operation 1 'draw-element))

(define (add-to-draw-element! predicate handler)
  (defhandler draw-element
    (lambda (element)
      (lambda (canvas)
        (handler canvas element)))
    predicate))

(define *point-radius* 0.02)
(define (draw-point canvas point)
  (canvas-set-color canvas "blue")
  (canvas-fill-circle canvas
               (point-x point)
               (point-y point)
               *point-radius*)
  (canvas-set-color canvas "black"))

(define (draw-segment canvas segment)
  (let ((p1 (segment-endpoint-1 segment))
        (p2 (segment-endpoint-2 segment)))
    (canvas-draw-line canvas
                      (point-x p1)
                      (point-y p1)
                      (point-x p2)
                      (point-y p2))))

(define (draw-line canvas line)
  (let ((p1 (line-p1 line)))
   (let ((p2 (add-to-point
              p1
              (vec-from-direction (line-direction line)))))
     (draw-segment canvas (extend-to-max-segment p1 p2)))))

(define (draw-ray canvas ray)
  (let ((p1 (ray-endpoint ray)))
    (let ((p2 (add-to-point
               p1
               (vec-from-direction (ray-direction ray)))))
      (draw-segment canvas (ray-extend-to-max-segment p1 p2)))))

(define (draw-circle canvas c)
  (let ((center (circle-center c))
        (radius (circle-radius c)))
    (canvas-draw-circle canvas
                        (point-x center)
                        (point-y center)
                        radius)))

(define *angle-mark-radius* 0.1)
(define (draw-angle canvas a)
  (let* ((vertex (angle-vertex a))
         (d1 (angle-arm-1 a))
         (d2 (angle-arm-2 a))
         (angle-start (direction-theta d2))
         (angle-end (direction-theta d1)))
    ;; Canvas arcs are drawn clockwise
    (canvas-draw-arc canvas
                     (point-x vertex)
                     (point-y vertex)
                     *angle-mark-radius*
                     angle-start
                     angle-end)))

;;; Add to generic operations

(add-to-draw-element! point? draw-point)
(add-to-draw-element! segment? draw-segment)
(add-to-draw-element! circle? draw-circle)
(add-to-draw-element! angle? draw-angle)
(add-to-draw-element! line? draw-line)
(add-to-draw-element! ray? draw-ray)

;;; Canvas for x-graphics

(define (x-graphics) (make-graphics-device 'x))

(define (canvas)
  (let ((g (x-graphics)))
    (graphics-enable-buffering g)
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

(define (canvas-set-color canvas color)
  (graphics-operation (canvas-g canvas) 'set-foreground-color color)
  )
