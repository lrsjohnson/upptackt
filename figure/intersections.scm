;;; intersections.scm --- Intersections

;;; Commentary:

;; Ideas:
;; - Unified intersections
;; - Separation of core computations

;; Future:
;; - Amb-like selection of multiple intersections, or list?
;; - Deal with elements that are exactly the same

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Computations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; line 1 through p1, p2 with line 2 through p3, p4
(define (intersect-lines-by-points p1 p2 p3 p4)
  (let ((x1 (point-x p1))
        (y1 (point-y p1))
        (x2 (point-x p2))
        (y2 (point-y p2))
        (x3 (point-x p3))
        (y3 (point-y p3))
        (x4 (point-x p4))
        (y4 (point-y p4)))
    (let* ((denom
            (det (det x1 1 x2 1)
                 (det y1 1 y2 1)
                 (det x3 1 x4 1)
                 (det y3 1 y4 1)))
           (num-x
            (det (det x1 y1 x2 y2)
                 (det x1  1 x2  1)
                 (det x3 y3 x4 y4)
                 (det x3  1 x4  1)))
           (num-y
            (det (det x1 y1 x2 y2)
                 (det y1  1 y2  1)
                 (det x3 y3 x4 y4)
                 (det y3  1 y4  1))))
      (if (= denom 0)
          '()
          (let
              ((px (/ num-x denom))
               (py (/ num-y denom)))
            (list (make-point px py)))))))

(define (intersect-circles-by-centers-radii c1 r1 c2 r2)
  (let* ((a (point-x c1))
         (b (point-y c1))
         (c (point-x c2))
         (d (point-y c2))
         (e (- c a))
         (f (- d b))
         (p (sqrt (+ (square e)
                     (square f))))
         (k (/ (- (+ (square p) (square r1))
                  (square r2))
               (* 2 p))))
    (if (> k r1)
        (error "Circle's don't intersect")
        (let* ((t (sqrt (- (square r1)
                           (square k))))
               (x1 (+ a (/ (* e k) p)))
               (y1 (+ b (/ (* f k) p)))
               (dx (/ (* f t) p))
               (dy (- (/ (* e t) p))))
          (list (make-point (+ x1 dx)
                            (+ y1 dy))
                (make-point (- x1 dx)
                            (- y1 dy)))))))

;;; Intersect circle centered at c with radius r and line through
;;; points p1, p2
(define (intersect-circle-line-by-points c r p1 p2)
  (let ((offset (sub-points (make-point 0 0) c)))
    (let ((p1-shifted (add-to-point p1 offset))
          (p2-shifted (add-to-point p2 offset)))
      (let ((x1 (point-x p1-shifted))
            (y1 (point-y p1-shifted))
            (x2 (point-x p2-shifted))
            (y2 (point-y p2-shifted)))
        (let* ((dx (- x2 x1))
               (dy (- y2 y1))
               (dr (sqrt (+ (square dx) (square dy))))
               (d (det x1 x2 y1 y2))
               (disc (- (* (square r) (square dr)) (square d))))
          (if (< disc 0)
              (list)
              (let ((x-a (* d dy))
                    (x-b (* (sgn dy) dx (sqrt disc)))
                    (y-a (- (* d dx)))
                    (y-b (* (abs dy) (sqrt disc))))
                (let ((ip1 (make-point
                            (/ (+ x-a x-b) (square dr))
                            (/ (+ y-a y-b) (square dr))))
                      (ip2 (make-point
                            (/ (- x-a x-b) (square dr))
                            (/ (- y-a y-b) (square dr)))))
                  (if (close-enuf? 0 disc) ;; Tangent
                      (list (add-to-point ip1 (reverse-vec offset)))
                      (list (add-to-point ip1 (reverse-vec offset))
                            (add-to-point ip2 (reverse-vec offset))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;; Basic Intersections ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (intersect-lines-to-list line1 line2)
  (let ((p1 (line-p1 line1))
        (p2 (line-p2 line1))
        (p3 (line-p1 line2))
        (p4 (line-p2 line2)))
    (intersect-lines-by-points p1 p2 p3 p4)))

(define (intersect-lines line1 line2)
  (let ((i-list (intersect-lines-to-list line1 line2)))
    (if (null? i-list)
        (error "Lines don't intersect")
        (car i-list))))

(define (intersect-circles cir1 cir2)
  (let ((c1 (circle-center cir1))
        (c2 (circle-center cir2))
        (r1 (circle-radius cir1))
        (r2 (circle-radius cir2)))
    (intersect-circles-by-centers-radii c1 r1 c2 r2)))

(define (intersect-circle-line cir line)
  (let ((center (circle-center cir))
        (radius (circle-radius cir))
        (p1 (line-p1 line))
        (p2 (line-p2 line)))
    (intersect-circle-line-by-points center radius p1 p2)))

(define standard-intersect
  (make-generic-operation 2 'standard-intersect))

(defhandler standard-intersect
  intersect-lines-to-list line? line?)

(defhandler standard-intersect
  intersect-circles circle? circle?)

(defhandler standard-intersect
  intersect-circle-line circle? line?)

(defhandler standard-intersect
  (flip-args intersect-circle-line) line? circle?)

;;;;;;;;;;;;;;;;;;;;;;;; Generic intersection ;;;;;;;;;;;;;;;;;;;;;;;;

(define (intersect-linear-elements el-1 el-2)
  (let ((i-list (standard-intersect (->line el-1)
                                    (->line el-2))))
    (if (null? i-list)
        #f
        (let ((i (car i-list)))
          (if (or (not (on-element? i el-1))
                  (not (on-element? i el-2)))
              #f
              i)))))

(define (intersect-linear-elements-no-endpoints el-1 el-2)
  (let ((i (intersect-linear-elements el-1 el-2)))
    (and (or i
             (element-endpoint? i el-1)
             (element-endpoint? i el-2))
         i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; On Elements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define on-element? (make-generic-operation 2 'on-element?))

(defhandler on-element? on-segment? point? segment?)
(defhandler on-element? on-line? point? line?)
(defhandler on-element? on-ray? point? ray?)
(defhandler on-element? on-circle? point? circle?)

;;;;;;;;;;;;;;;;;;;;;;; Element Endpoint Test ;;;;;;;;;;;;;;;;;;;;;;;;

(define element-endpoint? (make-generic-operation 2 'on-endpoint?
                                                  (lambda (p el) #f)))

(define (segment-endpoint? p seg)
  (or (point-equal? p (segment-endpoint-1 seg))
      (point-equal? p (segment-endpoint-2 seg))))
(defhandler element-endpoint? segment-endpoint? point? segment?)

(define (ray-endpoint? p ray)
  (point-equal? p (ray-endpoint seg)))
(defhandler element-endpoint? ray-endpoint? point? ray?)
