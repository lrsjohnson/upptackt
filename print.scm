
;;; print.scm --- Print things nicely

;;; Commentary:
;;; - Default printing is not very nice for many of our record structure

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Print ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define print
  (make-generic-operation 1 'print (lambda (x) x)))

(defhandler print
  (lambda (p) (cons (print (car p))
                    (print (cdr p))))
  pair?)

(defhandler print
  (lambda (l) (map print l))
  list?)

(define (pprint x)
  (pp (print x))
  (display "\n"))
