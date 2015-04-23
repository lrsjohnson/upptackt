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
