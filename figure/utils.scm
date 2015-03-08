;;; Tag Helper
(define ((tag-predicate tag) x)
  (and (pair? x)
       (eq? (car x) tag)))
