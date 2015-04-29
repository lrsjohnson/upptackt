(define (draw-figure figure canvas)
  (clear-canvas canvas)
  (for-each
   (lambda (element)
     (canvas-set-color canvas (element-color element))
     ((draw-element element) canvas))
   (figure-elements figure))
  (for-each
   (lambda (element)
     (canvas-set-color canvas (element-color element))
     ((draw-label element) canvas))
   (figure-elements figure))
  (graphics-flush (canvas-g canvas))
  )

(define draw-element
  (make-generic-operation 1 'draw-element))

(define draw-label
  (make-generic-operation 1 'draw-label (lambda (e) (lambda (c)'done))))

(define (add-to-draw-element! predicate handler)
  (defhandler draw-element
    (lambda (element)
      (lambda (canvas)
        (handler canvas element)))
    predicate))

(define (add-to-draw-label! predicate handler)
  (defhandler draw-label
    (lambda (element)
      (lambda (canvas)
        (handler canvas element)))
    predicate))


(define *point-radius* 0.02)
(define (draw-point canvas point)
  (canvas-fill-circle canvas
               (point-x point)
               (point-y point)
               *point-radius*))
(define (draw-point-label canvas point)
  (canvas-draw-text canvas
                    (+ (point-x point) *point-radius*)
                    (+ (point-y point) *point-radius*)
                    (symbol->string (element-name point))))

(define (draw-segment canvas segment)
  (let ((p1 (segment-endpoint-1 segment))
        (p2 (segment-endpoint-2 segment)))
    (canvas-draw-line canvas
                      (point-x p1)
                      (point-y p1)
                      (point-x p2)
                      (point-y p2))))
(define (draw-segment-label canvas segment)
  (let ((v (vec-from-direction-distance (rotate-direction-90
                                         (segment->direction segment))
                                        (* 2 *point-radius*)))
        (m (segment-midpoint segment)))
    (let ((label-point (add-to-point m v)))
      (canvas-draw-text canvas
                        (point-x label-point)
                        (point-y label-point)
                        (symbol->string (element-name segment))))))

(define (draw-line canvas line)
  (let ((p1 (line-p1 line)))
   (let ((p2 (add-to-point
              p1
              (unit-vec-from-direction (line-direction line)))))
     (draw-segment canvas (extend-to-max-segment p1 p2)))))

(define (draw-ray canvas ray)
  (let ((p1 (ray-endpoint ray)))
    (let ((p2 (add-to-point
               p1
               (unit-vec-from-direction (ray-direction ray)))))
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
    (canvas-draw-arc canvas
                     (point-x vertex)
                     (point-y vertex)
                     *angle-mark-radius*
                     angle-start
                     angle-end)))

(define (draw-polygon canvas polygon)
  (let ((points (polygon-points polygon)))
    (for-each (lambda (p)
                (draw-point canvas p))
              points)
    (for-each (lambda (s)
                (draw-segment canvas s))
              (polygon-segments polygon))))

;;; Add to generic operations

(add-to-draw-element! point? draw-point)
(add-to-draw-element! segment? draw-segment)
(add-to-draw-element! circle? draw-circle)
(add-to-draw-element! angle? draw-angle)
(add-to-draw-element! line? draw-line)
(add-to-draw-element! ray? draw-ray)
(add-to-draw-element! polygon? draw-polygon)

(add-to-draw-label! point? draw-point-label)

;;; Canvas for x-graphics

(define (x-graphics) (make-graphics-device 'x))

(define (canvas)
  (let ((g (x-graphics)))
    (graphics-enable-buffering g)
    (list 'canvas g)))

(define (canvas-g canvas)
  (cadr canvas))

(define (canvas? x)
  (and (pair? x)
       (eq? (car x 'canvas))))

(define (clear-canvas canvas)
  (graphics-clear (canvas-g canvas)))

(define (canvas-draw-circle canvas x y radius)
  (graphics-operation (canvas-g canvas)
                      'draw-circle
                      x y radius))

(define (canvas-draw-text canvas x y text)
  (graphics-draw-text (canvas-g canvas) x y text))

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
