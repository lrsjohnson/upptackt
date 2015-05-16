;;; observation.scm -- observed relationships

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Observation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <observation>
  (make-observation premises relationship args)
  observation?
  (premises observation-premises)
  (relationship observation-relationship)
  (args observation-args))



(define (print-observation obs)
  (cons
   (print (observation-relationship obs))
   (map element-dependencies->list (observation-args obs))))

(defhandler print print-observation observation?)


;;;;;;;;;;;;;;;;;;;;;;;; Checking observation ;;;;;;;;;;;;;;;;;;;;;;;;

(define (satisfies-observation obs new-premise)
  (let ((new-args (map (lambda (arg)
                         ((element-source arg) new-premise))
                       (observation-args obs)))
        (rel (observation-relationship obs)))
    (relationship-holds rel new-args)))

(define (satisfies-observations obs-list new-premise)
  (every (lambda (obs)
           (or (satisfies-observation obs new-premise)
               (begin (if *explain*
                          (pprint `(failed-observation ,obs)))
                      #f)))
         obs-list))
