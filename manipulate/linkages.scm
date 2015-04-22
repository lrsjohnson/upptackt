;;; Constraints between directions and coordinates

;;; Example
#|
 (let* ((s1 (m:make-bar))
        (s2 (m:make-bar))
        (j (m:make-joint)))
   (m:instantiate (m:joint-theta j) (/ pi 2) 'theta)
   (c:id (m:bar-length s1)
         (m:bar-length s2))
   (m:instantiate-point (m:bar-p2 s1) 4 0 'bar-2-endpoint)
   (m:instantiate-point (m:bar-p1 s1) 2 -2 'bar-2-endpoint)
   (m:identify-out-of-arm-2 j s2)
   (run)
   (m:examine-point (m:bar-p2 s2)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;; TMS Interfaces ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:instantiate cell value premise)
  (add-content cell
               ;(make-tms (contingent value (list premise)))
               value))

(define (m:examine-cell cell)
  (let ((v (content cell)))
    (cond ((nothing? v) v)
          ((tms? v)
           (contingent-info (tms-query v)))
          (else v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Dealing with Angles ;;;;;;;;;;;;;;;;;;;;;;

(propagatify fix-angle-0-2pi)

(define (ce:reverse-direction dir-cell)
  (let-cells (output-cell two-pi)
    ((constant (* 2 pi)) two-pi)
    (c:- two-pi dir-cell output-cell)
    output-cell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Vec ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type <m:vec>
  (%m:make-vec dx dy length direction)
  m:vec?
  (dx m:vec-dx)
  (dy m:vec-dy)
  (length m:vec-length)
  (direction m:vec-direction))

;;; Allocate and wire up the cells in a vec
(define (m:make-vec)
  (let-cells (dx dy length direction)
    (p:fix-angle-0-2pi
     (e:atan2 dy dx) direction)
    (p:sqrt (e:+ (e:square dx)
                 (e:square dy))
            length)
    (p:* length (e:cos direction) dx)
    (p:* length (e:sin direction) dy)
    (%m:make-vec dx dy length direction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type <m:point>
  (%m:make-point x y)
  m:point?
  (x m:point-x)
  (y m:point-y))

;;; Allocate cells for a point
(define (m:make-point)
  (let-cells (x y)
    (%m:make-point x y)))

(define (m:instantiate-point p x y premise)
  (m:instantiate (m:point-x p)
                 x premise)
  (m:instantiate (m:point-y p)
                 y premise))

(define (m:examine-point p)
  (list 'm:point
        (m:examine-cell (m:point-x p))
        (m:examine-cell (m:point-y p))))

;;; Set p1 and p2 to be equal
(define (m:identify-points p1 p2)
  (c:id (m:point-x p1)
        (m:point-x p2))
  (c:id (m:point-y p1)
        (m:point-y p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Bar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type <m:bar>
  (%m:make-bar p1 p2 vec)
  m:bar?
  (p1 m:bar-p1)
  (p2 m:bar-p2)
  (vec m:bar-vec))

(define (m:bar-direction bar)
  (m:vec-direction (m:bar-vec bar)))

(define (m:bar-length bar)
  (m:vec-length (m:bar-vec bar)))

;;; Allocate cells and wire up a bar
(define (m:make-bar)
  (let ((p1 (m:make-point))
        (p2 (m:make-point)))
   (let ((v (m:make-vec)))
     (c:+ (m:point-x p1)
          (m:vec-dx v)
          (m:point-x p2))
     (c:+ (m:point-y p1)
          (m:vec-dy v)
          (m:point-y p2))
     (%m:make-bar p1 p2 v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Joint  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Direction-2 is counter-clockwise from direction-1 by theta
(define-record-type <m:joint>
  (%m:make-joint vertex dir-1 dir-2 theta)
  m:joint?
  (vertex m:joint-vertex)
  (dir-1 m:joint-dir-1)
  (dir-2 m:joint-dir-2)
  (theta m:joint-theta))

(define (m:make-joint)
  (let ((vertex (m:make-point)))
    (let-cells (dir-1 dir-2 theta)
      (p:fix-angle-0-2pi
       (e:+ dir-1 theta)
       dir-2)
      (p:fix-angle-0-2pi
       (e:- dir-2 theta)
       dir-1)
      (p:fix-angle-0-2pi
       (e:- dir-2 dir-1)
       theta)
      (%m:make-joint vertex dir-1 dir-2 theta))))

(define (m:identify-out-of-arm-1 joint bar)
  (m:identify-points (m:joint-vertex joint)
                     (m:bar-p1 bar))
  (c:id (m:joint-dir-1 joint)
        (m:bar-direction bar)))

(define (m:identify-into-arm-1 joint bar)
  (m:identify-points (m:joint-vertex joint)
                     (m:bar-p2 bar))
  (c:id (m:joint-dir-1 joint)
        (e:reverse-direction (m:bar-direction bar))))

(define (m:identify-out-of-arm-2 joint bar)
  (m:identify-points (m:joint-vertex joint)
                     (m:bar-p1 bar))
  (c:id (m:joint-dir-2 joint)
        (m:bar-direction bar)))

(define (m:identify-into-arm-2 joint bar)
  (m:identify-points (m:joint-vertex joint)
                     (m:bar-p2 bar))
  (c:id (m:joint-dir-2 joint)
        (e:reverse-direction (m:bar-direction bar))))
