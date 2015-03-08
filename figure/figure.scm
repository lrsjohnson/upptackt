;;; Figure Structure

(define (figure . elements)
  (cons 'figure elements))
(define (figure-elements figure)
  (cdr figure))
(define figure? (tag-predicate 'figure))

(define (figure-filter predicate figure)
  (filter predicate (figure-elements figure)))
