;;; Assign name
(define (set-element-name! element name)
  (eq-put! element 'name name)
  element)

(define (element-name element)
  (or (eq-get element 'name)
      '*unnamed*))
