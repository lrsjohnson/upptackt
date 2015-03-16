;;; Assign name
(define (set-element-name! element name)
  (eq-put! element 'name name))

(define (element-name element)
  (or (eq-get element 'name)
      '*unnamed*))

(define (mark-known-equal seg1 seg2)
  (if (not (and (segment? seg1)
                (segment? seg2)))
      (error "can only mark segments as being equal"))
  (eq-put! seg1 'equal-segment seg2)
  (eq-put! seg2 'equal-segment seg1))
