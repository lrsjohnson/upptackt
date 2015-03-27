(define max-dist 2)

(define *g-min-x* -1)
(define *g-max-x*  1)
(define *g-min-y* -1)
(define *g-max-y*  1)

;;; Measurement-directed constructions

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
