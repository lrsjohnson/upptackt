(define (with-color color element)
  (eq-put! element 'color color)
  element)

(define (element-color element)
  (or (eq-get element 'color)
      "black"))
