;;; Assign name
(define (set-element-name! element name)
  (eq-put! element 'name name))

(define (element-name element)
  (eq-get element 'name))
