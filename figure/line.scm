;;; Segment, Line, Ray Types
(define-record-type <segment>
  (%segment p1 p2)
  segment?
  (p1 segment-endpoint-1)
  (p2 segment-endpoint-2))

(define-record-type <line>
  (%make-line point dir)
  line?
  (point line-point)
  (dir line-direction)) ;; Point on the line

(define make-line %make-line)

(define-record-type <ray>
  (make-ray initial-point direction)
  ray?
  (initial-point ray-endpoint)
  (direction ray-direction))

(define (linear-element? x)
  (or (line? x)
      (segment? x)
      (ray? x)))

;;; Alternate, helper constructors
(define (make-segment p1 p2)
  (let ((seg (%segment p1 p2)))
    (set-element-name!
     seg
     (symbol '*seg*: (element-name p1) '- (element-name p2)))
    seg))

;;; TODO, use for equality tests?
(define (line-offset line)
  (let ((direction (direction-from-points p2 p1))
        (x1 (point-x p1))
        (y1 (point-y p1))
        (x2 (point-x p2))
        (y2 (point-y p2)))
    (let ((offset (/ (- (* x2 y1)
                        (* y2 x1))
                     (distance p1 p2))))
      (%make-line direction offset))))

(define (line-from-points p1 p2)
  (make-line p1 (direction-from-points p2 p1)))

(define (line-from-point-direction p dir)
  (make-line p dir))

(define (ray-from-point-direction p dir)
  (make-ray p dir))

;;; Constructors from angles
(define (ray-from-arm-1 a)
  (let ((v (angle-vertex a))
        (dir (angle-arm-1 a)))
    (make-ray v dir)))

(define (ray-from-arm-2 a)
  (ray-from-arm-1 (reverse-angle a)))

(define (line-from-arm-1 a)
  (ray->line (ray-from-arm-1 a)))

(define (line-from-arm-2 a)
  (ray->line (ray-from-arm-2 a)))

;;; Transforms

(define flip (make-generic-operation 1 'flip))

(define (flip-line line)
  (make-line
   (line-point line)
   (reverse-direction (line-direction line))))
(defhandler flip flip-line line?)

(define (flip-segment s)
  (make-segment (segment-endpoint-2 s) (segment-endpoint-1 s)))
(defhandler flip flip-segment segment?)

;;; Operations

(define (segment-length seg)
  (distance (segment-endpoint-1 seg)
            (segment-endpoint-2 seg)))

(define (two-points-on-line line)
  (let ((point-1 (line-point line)))
   (let ((point-2 (add-to-point
                   point-1
                   (vec-from-direction (line-direction line)))))
     (list point-1 point-2))))

;;; TODO: Replace
(define (line-p1 line)
  (car (two-points-on-line line)))

(define (line-p2 line)
  (cadr (two-points-on-line line)))

;;; Predicates

(define (parallel? a b)
  (direction-parallel? (->direction a)
                       (->direction b)))

(define (perpendicular? a b)
  (direction-perpendicular? (->direction a)
                            (->direction b)))

(define (segment-equal? s1 s2)
  (and
   (point-equal? (segment-endpoint-1 s1)
                 (segment-endpoint-1 s2))
   (point-equal? (segment-endpoint-2 s1)
                 (segment-endpoint-2 s2))))

(define (segment-equal-ignore-direction? s1 s2)
  (or (segment-equal? s1 s2)
      (segment-equal? s1 (flip-segment s2))))

(define (segment-equal-length? seg-1 seg-2)
  (close-enuf? (segment-length seg-1)
               (segment-length seg-2)))

;;; Conversions
;;; Ray shares point p1
(define (segment->ray segment)
  (make-ray (segment-endpoint-1 segment)
            (direction-from-points
             (segment-endpoint-2 segment)
             (segment-endpoint-1 segment))))

(define (ray->line ray)
  (make-line (ray-endpoint ray)
             (ray-direction ray)))

(define (segment->line segment)
  (ray->line (segment->ray segment)))

(define (line->direction l)
  (line-direction l))

(define (ray->direction r)
  (ray-direction r))

(define (segment->direction s)
  (direction-from-points
   (segment-endpoint-2 s)
   (segment-endpoint-1 s)))

(define ->direction (make-generic-operation 1 '->direction))
(defhandler ->direction line->direction line?)
(defhandler ->direction ray->direction ray?)
(defhandler ->direction segment->direction segment?)
