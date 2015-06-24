;;; line.scm --- Line

;;; Commentary:

;; Ideas:
;; - Linear Elements: Segments, Lines, Rays
;; - All have direction
;; - Conversions to directions, extending.
;; - Lines are point + direction, but hard to access point
;; - Means to override dependencies for random segments

;; Future:
;; - Simplify direction requirements
;; - Improve some predicates, more tests
;; - Fill out more dependency information

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Segments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <segment>
  (make-segment p1 p2)
  segment?
  (p1 segment-endpoint-1)
  (p2 segment-endpoint-2))

(defhandler print
  (lambda (s)
    (if (named? s)
        (element-name s)
        `(*segment* ,(print (segment-endpoint-1 s))
                    ,(print (segment-endpoint-2 s)))))
  segment?)

(define (segment-endpoints s)
  (list (segment-endpoint-1 s)
        (segment-endpoint-2 s)))

#|
(define (set-segment-dependency! segment dependency)
  (set-dependency! segment dependency)
  (set-dependency!
   (segment-endpoint-1 segment)
   `(segment-endpoint-1 segment))
  (set-dependency!
   (segment-endpoint-2 segment)
   `(segment-endpoint-2 segment)))

;;; Alternate, helper constructors

(define (make-segment p1 p2)
  (let ((seg (%segment p1 p2)))
    (with-dependency
     `(segment ,p1 ,p2)
     seg)))
|#

(declare-element-component-handler
 (component-procedure-from-getters segment-endpoint-1
                                   segment-endpoint-2)
 segment?)

(defhandler generic-element-name
  (lambda (seg)
    `(*segment* ,(element-name (segment-endpoint-1 seg))
                ,(element-name (segment-endpoint-2 seg))))
  segment?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <line>
  (make-line point dir)
  line?
  (point line-point) ;; Point on the line
  (dir line-direction))

(defhandler print
  element-name
  line?)

(define (line-from-points p1 p2)
  (make-line p1 (direction-from-points p1 p2)))

(define (line-from-point-direction p dir)
  (make-line p dir))

(define (two-points-on-line line)
  (let ((point-1 (line-point line)))
   (let ((point-2 (add-to-point
                   point-1
                   (unit-vec-from-direction (line-direction line)))))
     (list point-1 point-2))))

(define (line-p1 line)
  (car (two-points-on-line line)))

(define (line-p2 line)
  (cadr (two-points-on-line line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <ray>
  (make-ray initial-point direction)
  ray?
  (initial-point ray-endpoint)
  (direction ray-direction))

(define (ray-from-point-direction p dir)
  (make-ray p dir))

(define (ray-from-points endpoint p1)
  (make-ray endpoint (direction-from-points endpoint p1)))

(define (reverse-ray ray)
  (make-ray
   (ray-endpoint ray)
   (reverse-direction (ray-direction ray))))

(define (shorten-ray-from-point r p)
  (if (not (on-ray? p r))
      (error "Can only shorten rays from points on the ray"))
  (ray-from-point-direction p (ray-direction r)))

;;;;;;;;;;;;;;;;;;;;;; Constructors from angles ;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Transforms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define flip (make-generic-operation 1 'flip))

(define (flip-line line)
  (make-line
   (line-point line)
   (reverse-direction (line-direction line))))
(defhandler flip flip-line line?)

(define (flip-segment s)
  (make-segment (segment-endpoint-2 s) (segment-endpoint-1 s)))
(defhandler flip flip-segment segment?)

(define (reverse-ray r)
  (make-ray (ray-endpoint r)
            (reverse-direction (ray-direction r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (segment-length seg)
  (distance (segment-endpoint-1 seg)
            (segment-endpoint-2 seg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (linear-element? x)
  (or (line? x)
      (segment? x)
      (ray? x)))

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

;;; Regardless of ordering or point naming, refers to the same pair of
;;; point locations.
(define (segment-equivalent? s1 s2)
  (set-equivalent?
   (segment-endpoints s1)
   (segment-endpoints s2)
   point-equal?))

(define (segment-equal-length? seg-1 seg-2)
  (close-enuf? (segment-length seg-1)
               (segment-length seg-2)))

(define (ray-equal? r1 r2)
  (and (point-equal?
        (ray-endpoint r1)
        (ray-endpoint r2))
       (direction-equal?
        (ray-direction r1)
        (ray-direction r2))))

;;; Ignores line point and direction
(define (line-equivalent? l1 l2)
  (and (or (on-line? (line-point l1) l2)
           (on-line? (line-point l2) l1))
       (or
        (direction-equal?
         (line-direction l1)
         (line-direction l2))
        (direction-opposite?
         (line-direction l1)
         (line-direction l2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Conversions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Ray shares point p1
(define (segment->ray segment)
  (make-ray (segment-endpoint-1 segment)
            (direction-from-points
             (segment-endpoint-1 segment)
             (segment-endpoint-2 segment))))

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
   (segment-endpoint-1 s)
   (segment-endpoint-2 s)))

(define (segment->vec s)
  (sub-points
   (segment-endpoint-2 s)
   (segment-endpoint-1 s)))

(define ->direction (make-generic-operation 1 '->direction))
(defhandler ->direction line->direction line?)
(defhandler ->direction ray->direction ray?)
(defhandler ->direction segment->direction segment?)

(define ->line (make-generic-operation 1 '->line))
(defhandler ->line identity line?)
(defhandler ->line segment->line segment?)
(defhandler ->line ray->line ray?)

(define linear-element-equivalent?
  (make-generic-operation 2 'linear-element-equivalent?
                          false-proc))

(defhandler linear-element-equivalent?
  segment-equivalent?
  segment? segment?)

(defhandler linear-element-equivalent?
  ray-equal?
  ray? ray?)

(defhandler linear-element-equivalent?
  line-equivalent?
  line? line?)
