(define (with-color color element)
  (eq-put! element 'color color)
  element)

(define default-element-color
  (make-generic-operation 1
                          'default-element-color
                          (lambda (e) "black")))

(defhandler default-element-color (lambda (e) "blue") point?)
(defhandler default-element-color (lambda (e) "black") segment?)

(define (element-color element)
  (or (eq-get element 'color)
      (default-element-color element)))
