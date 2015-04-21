;;; Assign name
(define (set-element-name! element name)
  (eq-put! element 'name name)
  element)

(define (element-name element)
  (or (eq-get element 'name)
      '*unnamed*))

;;; Dependencies:
(define (set-dependency! element dependency)
  (eq-put! element 'dependency dependency))

(define (with-dependency dependency element)
  (set-dependency! element dependency)
  element)

(define (with-dependency-if-unknown dependency element)
  (if (dependency-unknown? element)
      (with-dependency dependency element)
      element))

(define *unknown-dependency* (list '*unknown-dependency*))
(define (unknown-dependency? x)
  (eq? x *unknown-dependency*))

(define (dependency-unknown? element)
  (unknown-dependency? (element-dependency element)))

(define (element-dependency element)
  (or (eq-get element 'dependency)
      *unknown-dependency*))


;;; Initial attempt at dependencies:
(define (mark-known-equal seg1 seg2)
  (if (not (and (segment? seg1)
                (segment? seg2)))
      (error "can only mark segments as being equal"))
  (eq-put! seg1 'equal-segment seg2)
  (eq-put! seg2 'equal-segment seg1))
