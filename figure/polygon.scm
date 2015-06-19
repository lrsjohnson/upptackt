;;; polygon.scm --- Polygons

;;; Commentary:

;; Ideas:
;; - Points and (derived) segments define polygon

;; Future
;; - Figure out dependencies better
;; - Other operations, angles? diagonals? etc.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;; Polygon Structure ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Data structure for a polygon, implemented as a list of
;;; points in counter-clockwise order.
;;; Drawing a polygon will draw all of its points and segments.
(define-record-type <polygon>
  (%polygon n-points points)
  polygon?
  (n-points polygon-n-points)
  (points %polygon-points))

(define (polygon-from-points . points)
  (let ((n-points (length points)))
    (%polygon n-points points)))

(define ((ngon-predicate n) obj)
  (and (polygon? obj)
       (= n (polygon-n-points obj))))

(defhandler print
  (lambda (p)
    (if (named? p)
        (element-name p)
        `(*polygon* ,@(map print (polygon-points p)))))
  polygon?)


;;;;;;;;;;;;;;;;;;;;;;;;;;; Polygon Points ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Internal reference for polygon points
(define (polygon-point-ref polygon i)
  (if (not (<= 0 i (- (polygon-n-points polygon) 1)))
      (error "polygon point index not in range"))
  (list-ref (%polygon-points polygon) i))

(define (polygon-points polygon)
  (map (lambda (i) (polygon-point polygon i))
       (iota (polygon-n-points polygon))))

;;; External polygon points including dependencies
(define (polygon-point polygon i)
  (with-dependency ;;-if-unknown
   `(polygon-point ,i ,(element-dependency polygon))
   (with-source
    (lambda (p) (polygon-point (car p) i))
    (polygon-point-ref polygon i))))

(declare-element-component-handler
 polygon-point
 polygon?)

(define (polygon-index-from-point polygon point)
  (index-of
   point
   (%polygon-points polygon)
   point-equal?))

(define (name-polygon polygon)
  (for-each (lambda (i)
              (set-element-name! (polygon-point-ref polygon i)
                                 (nth-letter-symbol (+ i 1))))
            (iota (polygon-n-points polygon)))
  polygon)

;;;;;;;;;;;;;;;;;;;;;;;;;; Polygon Segments ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; i and j are indices of adjacent points
(define (polygon-segment polygon i j)
  (let ((n-points (polygon-n-points polygon)))
   (cond
    ((not (or (= i (modulo (+ j 1) n-points))
              (= j (modulo (+ i 1) n-points))))
     (error "polygon-segment must be called with adjacent indices"))
    ((or (>= i n-points)
         (>= j n-points))
     (error "polygon-segment point index out of range"))
    (else
     (let* ((p1 (polygon-point-ref polygon i))
            (p2 (polygon-point-ref polygon j))
            (segment (make-segment p1 p2)))
       ;;: TODO: Handle situations where polygon isn't terminal dependency
       segment)))))

(define (polygon-segments polygon)
  (let ((n-points (polygon-n-points polygon)))
    (map (lambda (i)
           (let ((j (modulo (+ i 1) n-points)))
            (with-dependency
             `(polygon-segment ,polygon ,i ,j)
             (with-source
              (lambda (p)
                (polygon-segment (from-new-premise p polygon)
                                 i j))
              (polygon-segment polygon i j)))))
         (iota n-points))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Polygon Angles ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define polygon-angle
  (make-generic-operation 2 'polygon-angle))

(define (polygon-angle-by-index polygon i)
  (let ((n-points (polygon-n-points polygon)))
    (cond
     ((not (<= 0 i (- n-points 1)))
      (error "polygon-angle point index out of range"))
     (else
      (let* ((v (polygon-point-ref polygon i))
             (a1p (polygon-point-ref polygon
                                     (modulo (- i 1)
                                             n-points)))
             (a2p (polygon-point-ref polygon
                                     (modulo (+ i 1)
                                             n-points)))
             (angle (angle-from-points a1p v a2p)))
        angle)))))

(defhandler polygon-angle
  polygon-angle-by-index
  polygon? number?)

(define (polygon-angle-by-point polygon p)
  (let ((i (polygon-index-from-point polygon p)))
    (if (not i)
        (error "Point not in polygon" (list p polygon)))
    (polygon-angle-by-index polygon i)))

(defhandler polygon-angle
  polygon-angle-by-point
  polygon? point?)

(define (polygon-angles polygon)
  (map (lambda (i)
         (with-dependency
          `(polygon-angle ,polygon ,i)
          (with-source
           (lambda (p)
             (polygon-angle-by-index
              (from-new-premise p polygon) i))
           (polygon-angle-by-index polygon i))))
       (iota (polygon-n-points polygon))))
