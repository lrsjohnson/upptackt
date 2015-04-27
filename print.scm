;;; print.scm --- Print things nicely

;;; Commentary:
;;; - Default printing is not very nice for many of our record structure

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Print ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define print
  (make-generic-operation 1 'print (lambda (x) x)))

(define (pprint x)
  (pp (print x)))
