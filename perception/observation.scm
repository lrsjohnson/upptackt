;;; observation.scm -- observed relationships

;;; Commentary:

;; Future:
;; - Observation equality is more complicated!

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Observation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <observation>
  (make-observation relationship args)
  observation?
  (relationship observation-relationship)
  (args observation-args))

(define (observation-equal? obs1 obs2)
  (equal? (print-observation obs1)
          (print-observation obs2)))

(define (print-observation obs)
  (cons
   (print (observation-relationship obs))
   (map print (observation-args obs))))
(defhandler print print-observation observation?)
