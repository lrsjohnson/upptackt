;;; Playing around with propagators

;;; Constructors

;;; Vectors
(define-record-type <p-vec>
  (%make-p-vec dx dy length direction)
  p-vec?
  (dx p-vec-x)
  (dy p-vec-y)
  (length p-vec-length)
  (direction p-vec-direction))

(define (make-p-vec)
  (let-cells (dx dy length direction)
    (p:atan2 dy dx direction)
    (p:sqrt (e:+ (e:square dx)
                 (e:square dy)) length)
    (p:* length (e:cos direction) dx)
    (p:* length (e:sin direction) dy)
    (add-content length (make-interval 0 1000))
    (%make-p-vec dx dy length direction)))

;;; Points
(define-record-type <p-point>
  (%make-p-point x y)
  p-point?
  (x p-point-x)
  (y p-point-y))

(define (make-p-point)
  (let-cells (x y)
    (%make-p-point x y)))

(define (instantiate-p-point p x y)
  (add-content (p-point-x p) x)
  (add-content (p-point-y p) y))

(define (examine-p-point p)
  (list (content (p-point-x p))
        (content (p-point-y p))))

(define (examine-p-segment s)
  (list 'length (content (p-segment-length s))
        'direction (content (p-segment-direction s))))

;;; Segments
(define-record-type <p-segment>
  (%make-p-segment p1 p2 length direction)
  p-segment?
  (p1 p-segment-p1)
  (p2 p-segment-p2)
  (length p-segment-length)
  (direction p-segment-direction))

(define (make-p-segment p1 p2)
  (let ((segment-vec (make-p-vec)))
    (c:add-vec-to-point p1 segment-vec p2)
    (%make-p-segment p1 p2 (p-vec-length segment-vec) (p-vec-direction segment-vec))))


;;; Operations


(define (p-midpoint s)
  (let ((p1 (p-segment-p1 s))
        (p2 (p-segment-p2 s)))
   (let ((newpoint (make-p-point)))
     (let ((s1 (make-p-segment p1 newpoint))
           (s2 (make-p-segment newpoint p2)))
       (c:id (p-segment-length s1)
             (p-segment-length s2))
       (c:+ (p-segment-length s1)
            (p-segment-length s2)
            (p-segment-length s))
       (c:== (p-segment-direction s)
             (p-segment-direction s2)
             (p-segment-direction s2)))
     newpoint)))


;;; Utility constraints and propagators

(define (c:add-vec-to-point p1 vec p2)
  (let ((p1-x (p-point-x p1))
        (p1-y (p-point-y p1))
        (dx (p-vec-x vec))
        (dy (p-vec-y vec))
        (p2-x (p-point-x p2))
        (p2-y (p-point-y p2)))
    (c:+ p1-x dx p2-x)
    (c:+ p1-y dy p2-y)))

(define-d:propagator (c:average a b c)
  (c:/ (ce:+ a b) 2.0 c))
