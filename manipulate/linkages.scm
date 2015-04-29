;;; linkages.scm ---  Bar/Joint propagators between directions and coordinates

;;; Commentary:

;; Ideas:
;; - Join "Identify" bars and joints to build mechanism
;;   versions of diagrams
;; - Use propagator system to deal with partial information
;; - Used Regions for partial info about points,
;; - Direction Intervals for partial info about joint directions.

;; Future:
;; - Other Linkages?
;; - Draw partially assembled linkages

;;; Example:

#|
 (let* ((s1 (m:make-bar))
        (s2 (m:make-bar))
        (j (m:make-joint)))
   (m:instantiate (m:joint-theta j) (/ pi 2) 'theta)
   (c:id (m:bar-length s1)
         (m:bar-length s2))
   (m:instantiate-point (m:bar-p2 s1) 4 0 'bar-2-endpoint)
   (m:instantiate-point (m:bar-p1 s1) 2 -2 'bar-2-endpoint)
   (m:identify-out-of-arm-1 j s1)
   (m:identify-out-of-arm-2 j s2)
   (run)
   (m:examine-point (m:bar-p2 s2)))
|#

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;; TMS Interfaces ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:instantiate cell value premise)
  (add-content cell
               ;;(make-tms (contingent value (list premise)))
               value))

(define (m:examine-cell cell)
  (let ((v (content cell)))
    (cond ((nothing? v) v)
          ((tms? v)
           (contingent-info (tms-query v)))
          (else v))))

(defhandler print
  (lambda (cell) (print (m:examine-cell cell)))
  cell?)

;;;;;;;;;;;;;;;;;;;;;;;; Reversing directions ;;;;;;;;;;;;;;;;;;;;;;;;

(define m:reverse-direction
  (make-generic-operation 1 'm:reverse-direction))
(defhandler m:reverse-direction
  reverse-direction direction?)
(defhandler m:reverse-direction
  reverse-direction-interval direction-interval?)

(propagatify m:reverse-direction)

(define (ce:reverse-direction input-cell)
  (let-cells (output-cell)
    (p:m:reverse-direction input-cell output-cell)
    (p:m:reverse-direction output-cell input-cell)
    output-cell))

(define m:add-to-direction
  (make-generic-operation 2 'm:add-to-direction))
(defhandler m:add-to-direction
   add-to-direction direction? number?)
(defhandler m:add-to-direction
   shift-direction-interval direction-interval? number?)

(propagatify m:add-to-direction)

(define m:subtract-directions
  (make-generic-operation 2 'm:subtract-directions))
(defhandler m:subtract-directions
  subtract-directions direction? direction?)
;;; TODO: Support Intervals for thetas
(defhandler m:subtract-directions
  (lambda (a b) nothing)
  direction-interval? direction-interval?)
(defhandler m:subtract-directions
  (lambda (a b) nothing)
  direction-interval? direction?)
(defhandler m:subtract-directions
  (lambda (a b) nothing)
  direction? direction-interval?)

(propagatify m:subtract-directions)

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
    (p:make-direction
     (e:atan2 dy dx) direction)
    (p:sqrt (e:+ (e:square dx)
                 (e:square dy))
            length)
    (p:* length (e:cos direction) dx)
    (p:* length (e:sin direction) dy)
    (%m:make-vec dx dy length direction)))

(define (m:print-vec v)
  `(m:vec (,(print (m:vec-dx v))
           ,(print (m:vec-dy v)))
          ,(print (m:vec-length v))
          ,(print (m:vec-direction v))))

(defhandler print m:print-vec m:vec?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type <m:point>
  (%m:make-point x y region)
  m:point?
  (x m:point-x)
  (y m:point-y)
  (region m:point-region))

;;; Allocate cells for a point
(define (m:make-point)
  (let-cells (x y region)
    (m:x-y->region-propagator x y region)
    (m:region->x-y-propagator region x y)
    (%m:make-point x y region)))

(define (m:x-y->region-propagator x y region)
  (propagator (list x y)
    (lambda ()
      (let ((x-value (content x))
            (y-value (content y)))
        (if (and (not (nothing? x-value))
                 (not (nothing? y-value)))
            (add-content
             region
             (m:make-singular-point-set (make-point x-value y-value)))
            'done)))))

(define (m:region->x-y-propagator region x y)
  (propagator (list region)
    (lambda ()
      (let ((region-value (content region)))
        (if (m:singular-point-set? region-value)
            (let ((p (m:singular-point-set-point region-value)))
              (add-content x (point-x p))
              (add-content y (point-y p)))
            'done)))))

(define (m:instantiate-point p x y premise)
  (m:instantiate (m:point-x p)
                 x premise)
  (m:instantiate (m:point-y p)
                 y premise)
  (m:instantiate (m:point-region p)
                 (m:make-singular-point-set (make-point x y))
                 premise))

(define (m:examine-point p)
  (list 'm:point
        (m:examine-cell (m:point-x p))
        (m:examine-cell (m:point-y p))))

(define (m:print-point p)
  `(m:point ,(print (m:point-x p))
            ,(print (m:point-y p))
            ,(print (m:point-region p))))

(defhandler print m:print-point m:point?)

;;; Set p1 and p2 to be equal
(define (m:identify-points p1 p2)
  (for-each (lambda (getter)
              (c:id (getter p1)
                    (getter p2)))
            (list m:point-x m:point-y m:point-region)))

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

(define (m:print-bar b)
  `(m:bar
    ,(m:bar-name b)
    ,(print (m:bar-p1 b))
    ,(print (m:bar-p2 b))
    ,(print (m:bar-vec b))))

(defhandler print m:print-bar m:bar?)

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
      (let ((bar (%m:make-bar p1 p2 v)))
        (m:p1->p2-bar-propagator p1 p2 bar)
        (m:p2->p1-bar-propagator p2 p1 bar)
        bar))))

;;; TODO: Combine p1->p2 / p2->p1
(define (m:p1->p2-bar-propagator p1 p2 bar)
  (let ((p1x (m:point-x p1))
        (p1y (m:point-y p1))
        (p2r (m:point-region p2))
        (length (m:bar-length bar))
        (dir (m:bar-direction bar)))
    (propagator (list p1x p1y length dir)
      (lambda ()
        (let ((x-value (content p1x))
              (y-value (content p1y))
              (len-value (content length))
              (dir-value (content dir)))
          (if (or (nothing? x-value)
                  (nothing? y-value))
              'done
              (let ((vertex (make-point x-value y-value)))
                (cond ((and (not (nothing? len-value))
                            (not (nothing? dir-value))
                            (direction-interval? dir-value))
                       (add-content
                        p2r
                        (m:make-arc
                         vertex len-value
                         dir-value)))
                      ((and (not (nothing? dir-value))
                            (direction? dir-value))
                       (add-content
                        p2r
                        (m:make-ray
                         vertex
                         dir-value)))))))))))

(define (m:p2->p1-bar-propagator p1 p2 bar)
  (let ((p1x (m:point-x p1))
        (p1y (m:point-y p1))
        (p2r (m:point-region p2))
        (length (m:bar-length bar))
        (dir (m:bar-direction bar)))
    (propagator (list p1x p1y length dir)
      (lambda ()
        (let ((x-value (content p1x))
              (y-value (content p1y))
              (len-value (content length))
              (dir-value (content dir)))
          (if (or (nothing? x-value)
                  (nothing? y-value))
              'done
              (let ((vertex (make-point x-value y-value)))
                (cond ((and (not (nothing? len-value))
                            (not (nothing? dir-value))
                            (direction-interval? dir-value))
                       (add-content
                        p2r
                        (m:make-arc
                         vertex len-value
                         (reverse-direction-interval dir-value))))
                      ((and (not (nothing? dir-value))
                            (direction? dir-value))
                       (add-content
                        p2r
                        (m:make-ray
                         vertex
                         (reverse-direction dir-value))))))))))))

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
      (p:m:add-to-direction
       dir-1 theta dir-2)
      (p:m:add-to-direction
       dir-2 (e:negate theta) dir-1)
      (p:m:subtract-directions
       dir-2 dir-1
       theta)
      (%m:make-joint vertex dir-1 dir-2 theta))))

(define (m:print-joint j)
  `(m:joint
    ,(m:joint-name j)
    ,(print (m:joint-dir-1 j))
    ,(print (m:joint-vertex j))
    ,(print (m:joint-dir-2 j))
    ,(print (m:joint-theta j))))

(defhandler print m:print-joint m:joint?)

(define (m:propagate-to-dir
         min-dir-value-cell
         bar-dir-cell)
  (propagator (list min-dir-value-cell)
    (lambda ()
      (let ((min-dir-value (content min-dir-value-cell)))
        (if (and (not (nothing? min-dir-value))
                 (direction? min-dir-value))
            (add-content
             bar-dir-cell
             (make-semi-circle-direction-interval
              min-dir-value))
            'done)))))

;;; TOOD: Abstract?
(define (m:identify-out-of-arm-1 joint bar)
  (m:identify-points (m:joint-vertex joint)
                     (m:bar-p1 bar))
  (c:id (m:joint-dir-1 joint)
        (m:bar-direction bar))
  (m:propagate-to-dir
   (ce:reverse-direction (m:joint-dir-2 joint))
   (m:bar-direction bar)))

(define (m:identify-out-of-arm-2 joint bar)
  (m:identify-points (m:joint-vertex joint)
                     (m:bar-p1 bar))
  (c:id (m:joint-dir-2 joint)
        (m:bar-direction bar))
  (m:propagate-to-dir
   (m:joint-dir-1 joint)
   (m:bar-direction bar)))

(define (m:identify-into-arm-1 joint bar)
  (m:identify-points (m:joint-vertex joint)
                     (m:bar-p2 bar))
  (c:id (ce:reverse-direction (m:joint-dir-1 joint))
        (m:bar-direction bar))
  (m:propagate-to-dir
   (m:joint-dir-2 joint)
   (m:bar-direction bar)))

(define (m:identify-into-arm-2 joint bar)
  (m:identify-points (m:joint-vertex joint)
                     (m:bar-p2 bar))
  (c:id (ce:reverse-direction (m:joint-dir-2 joint))
        (m:bar-direction bar))
  (m:propagate-to-dir
   (ce:reverse-direction (m:joint-dir-1 joint))
   (m:bar-direction bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Named Linkages  ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:name-element! element name)
  (eq-put! element 'm:name name))

(define (m:element-name element)
  (or (eq-get element 'm:name)
      '*unnamed*))

(define (m:make-named-bar p1-name p2-name)
  (let ((bar (m:make-bar)))
    (m:name-element! (m:bar-p1 bar) p1-name)
    (m:name-element! (m:bar-p2 bar) p2-name)
    bar))

(define (m:bar-name bar)
  (m:make-bar-name-key
   (m:element-name (m:bar-p1 bar))
   (m:element-name (m:bar-p2 bar))))

(define (m:bar-p1-name bar)
  (m:element-name (m:bar-p1 bar)))

(define (m:bar-p2-name bar)
  (m:element-name (m:bar-p2 bar)))

(define (m:make-named-joint arm-1-name vertex-name arm-2-name)
  (let ((joint (m:make-joint)))
    (m:name-element! (m:joint-dir-1 joint) arm-1-name)
    (m:name-element! (m:joint-vertex joint) vertex-name)
    (m:name-element! (m:joint-dir-2 joint) arm-2-name)
    joint))

(define (m:joint-name joint)
  (m:make-joint-name-key
   (m:joint-dir-1-name joint)
   (m:joint-vertex-name joint)
   (m:joint-dir-2-name joint)))

(define (m:joint-vertex-name joint)
  (m:element-name (m:joint-vertex joint)))

(define (m:joint-dir-1-name joint)
  (m:element-name (m:joint-dir-1 joint)))

(define (m:joint-dir-2-name joint)
  (m:element-name (m:joint-dir-2 joint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Keys for Names ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:make-bar-name-key p1-name p2-name)
  (symbol 'm:bar: p1-name ': p2-name))

(define (m:make-joint-name-key dir-1-name vertex-name dir-2-name)
  (symbol 'm:joint: dir-1-name ': vertex-name ': dir-2-name))

;;;;;;;;;;;;;;;;;; Table and operations using names ;;;;;;;;;;;;;;;;;;

(define (m:identify-joint-bar-by-name joint bar)
  (let ((vertex-name (m:joint-vertex-name joint))
        (dir-1-name (m:joint-dir-1-name joint))
        (dir-2-name (m:joint-dir-2-name joint))
        (bar-p1-name (m:bar-p1-name bar))
        (bar-p2-name (m:bar-p2-name bar)))
    (cond ((eq? vertex-name bar-p1-name)
           (cond ((eq? dir-1-name bar-p2-name)
                  (m:identify-out-of-arm-1 joint bar))
                 ((eq? dir-2-name bar-p2-name)
                  (m:identify-out-of-arm-2 joint bar))
                 (else (error "Bar can't be identified with joint - no arm"
                              bar-p2-name))))
          ((eq? vertex-name bar-p2-name)
           (cond ((eq? dir-1-name bar-p1-name)
                  (m:identify-into-arm-1 joint bar))
                 ((eq? dir-2-name bar-p1-name)
                  (m:identify-into-arm-2 joint bar))
                 (else (error "Bar can't be identified with joint - no arm"
                              bar-p1-name))))
          (else (error "Bar can't be identified with joint - no vertex"
                       vertex-name)))))

(define (m:make-bars-by-name-table bars)
  (let ((table (make-key-weak-eqv-hash-table)))
    (for-each (lambda (bar)
                (hash-table/put! table
                                 (m:bar-name bar)
                                 bar))
              bars)
    table))

(define (m:find-bar-by-unordered-endpoint-names table p1-name p2-name)
  (or (hash-table/get table
                      (m:make-bar-name-key p1-name p2-name)
                      #f)
      (hash-table/get table
                      (m:make-bar-name-key p2-name p1-name)
                      #f)))

(define (m:make-joints-by-vertex-name-table joints)
  (let ((table (make-key-weak-eq-hash-table)))
    (for-each (lambda (joint)
                (hash-table/put! table
                                 (m:joint-vertex-name joint)
                                 joint))
              joints)
    table))

;;;;;;;;;;;;;;;;;;;;;;;;; Degrees of Freedom  ;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:specified? cell #!optional predicate)
  (let ((v (m:examine-cell cell)))
    ;(and
     (not (nothing? v))
     ;(or (default-object? predicate)
     ;(predicate v))))
     ))

(define (m:bar-length-specified? bar)
  (m:specified? (m:bar-length bar)) number?)

(define (m:bar-direction-specified? bar)
  (m:specified? (m:bar-direction bar)) direction?)

(define (m:joint-theta-specified? joint)
  (m:specified? (m:joint-theta joint)) number?)

;;;;;;;;;;;;;;;;;;;;;;;;;; Point Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:point-specified? p)
  (and (m:specified? (m:point-x p) number?)
       (m:specified? (m:point-y p) number?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Bar Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:bar-p1-specified? bar)
  (m:point-specified? (m:bar-p1 bar)))

(define (m:bar-p2-specified? bar)
  (m:point-specified? (m:bar-p2 bar)))

(define (m:bar-anchored? bar)
  (or (m:bar-p1-specified? bar)
      (m:bar-p2-specified? bar)))

(define (m:bar-directioned? bar)
  (and (m:bar-anchored? bar)
       (m:specified? (m:bar-direction bar) direction?)))

(define (m:bar-length-specified? bar)
  (and (m:specified? (m:bar-length bar) number?)))

(define (m:bar-fully-specified? bar)
  (and (m:bar-p1-specified? bar)
       (m:bar-p2-specified? bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Joint Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:joint-dir-1-specified? joint)
  (m:specified? (m:joint-dir-1 joint) direction?))

(define (m:joint-dir-2-specified? joint)
  (m:specified? (m:joint-dir-2 joint) direction?))

(define (m:joint-anchored? joint)
  (or (m:joint-dir-1-specified? joint)
      (m:joint-dir-2-specified? joint)))

(define (m:joint-specified? joint)
  (m:specified? (m:joint-theta joint) number?))

(define (m:joint-fully-specified? joint)
  (and
   (m:point-specified? (m:joint-vertex joint))
   (m:joint-dir-1-specified? joint)
   (m:joint-dir-2-specified? joint)))

;;;;;;;;;;;;;;;;;;;;;;;;; Specifying Values ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:random-joint-theta)
  (internal-rand-range
   (* 1 (/ pi 10))
   (* 4 (/ pi 10))))

(define (m:random-bar-length)
  (internal-rand-range 0.1 1.9))

(define (m:initialize-bar bar)
  (m:instantiate-point (m:bar-p1 bar) 0 0 'initialize)
  (m:instantiate (m:bar-direction bar) (make-direction 0) 'initialize)
  (let ((v (m:random-bar-length)))
    (pp `(initializing-bar ,(m:bar-name bar) ,v))
    (m:instantiate (m:bar-length bar) v 'initialize)))

(define (m:initialize-joint joint)
  (m:instantiate-point (m:joint-vertex joint) 0 0 'initialize)
  (m:instantiate (m:joint-dir-1 joint) (make-direction 0) 'initialize))

;;;;;;;;;; Assembling named joints into diagrams ;;;;;;;

(define (m:assemble-linkages bars joints)
  (let ((bar-table (m:make-bars-by-name-table bars)))
    (for-each
     (lambda (joint)
       (let ((vertex-name (m:joint-vertex-name joint))
             (dir-1-name (m:joint-dir-1-name joint))
             (dir-2-name (m:joint-dir-2-name joint)))
         (for-each
          (lambda (dir-name)
            (let ((bar (m:find-bar-by-unordered-endpoint-names
                        bar-table
                        vertex-name
                        dir-name)))
              (if (eq? bar #f)
                  (error "Could not find bar for" vertex-name dir-name))
              (m:identify-joint-bar-by-name joint bar)))
                   (list dir-1-name dir-2-name))))
     joints)))

#|
 ;; Simple example of "solving for the third point"
 (begin
   (initialize-scheduler)
   (let ((b1 (m:make-named-bar 'a 'c))
         (b2 (m:make-named-bar 'b 'c))
         (b3 (m:make-named-bar 'a 'b))
         (j1 (m:make-named-joint 'b 'a 'c))
         (j2 (m:make-named-joint 'c 'b 'a))
         (j3 (m:make-named-joint 'a 'c 'b)))

     (m:assemble-linkages
      (list b1 b2 b3)
      (list j2 j3 j1))

     (m:initialize-joint j1)
     (c:id (m:bar-length b1) (m:bar-length b2))

     (m:instantiate (m:bar-length b3) 6 'b3-len)
     (m:instantiate (m:bar-length b1) 5 'b1-len)
     (run)
     (m:examine-point (m:bar-p2 b1))))
 ;Value: (m:point 3 4)

|#

;;;;;;;;;;;;;;;;;;; Converstion to Figure Elements ;;;;;;;;;;;;;;;;;;;

;;; TODO: Extract dependencies from TMS? or set names

(define (m:point->figure-point m-point)
  (if (not (m:point-specified? m-point))
      (error "Cannot convert non-specified point to figure point"))
  (let ((p (make-point (m:examine-cell (m:point-x m-point))
                       (m:examine-cell (m:point-y m-point)))))
    (set-element-name! p (m:element-name m-point))
    p))

(define (m:bar->figure-segment m-bar)
  (if (not (m:bar-fully-specified? m-bar))
      (error "Cannot convert non-fully specified bar to segment"))
  (make-segment (m:point->figure-point (m:bar-p1 m-bar))
                (m:point->figure-point (m:bar-p2 m-bar))))

(define (m:joint->figure-angle m-joint)
  (if (not (m:joint-fully-specified? m-joint))
      (error "Cannot convert non-fully specified joint to angle"))
  (make-angle (m:examine-cell (m:joint-dir-2 m-joint))
              (m:point->figure-point (m:joint-vertex m-joint))
              (m:examine-cell (m:joint-dir-1 m-joint))))
