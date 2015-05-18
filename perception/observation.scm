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

(define (observation-equal? obs1 obs2)
  (equal? (print-observation obs1)
          (print-observation obs2)))

(define (print-observation obs)
  (cons
   (print (observation-relationship obs))
   (map element-dependencies->list (observation-args obs))))

(defhandler print print-observation observation?)


;;;;;;;;;;;;;;;;;;;;;;;; Checking observation ;;;;;;;;;;;;;;;;;;;;;;;;

(define (satisfies-observation obs new-premise)
  (let ((new-args
         (map (lambda (arg)
                ((element-source arg) new-premise))
              (observation-args obs)))
        (rel (observation-relationship obs)))
    (or (relationship-holds rel new-args)
        (begin (if *explain*
                   (pprint `(failed-observation ,obs)))
               #f))))

;;;;;;;;;;;;;;;;;;;;;; Simplifying observations ;;;;;;;;;;;;;;;;;;;;;;

(define (simplify-observations observations base-observations)
  (define memp (member-procedure observation-equal?))
  (filter
   (lambda (o) (not (memp o base-observations)))
   observations))
