;; conjecture -- a proposed conjecture based on an observed relationship

;;; Commentary

;; Ideas:
;; - Higher-level than raw observations reported by perception/analyzer

;; Future:
;; - More complicated premises
;; - "Pattern-matching"

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Conjecture ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <conjecture>
  (make-conjecture premises constructions construction-procedures
                   relationship)
  conjecture?
  (premises conjecture-premises)
  (constructions conjecture-constructions)
  (construction-procedures conjecture-construction-procedures)
  (relationship conjecture-relationship))

(define (print-conjecture conj)
  (cons
   (print (conjecture-relationship conj))
   (conjecture-constructions conj)))

(defhandler print print-conjecture conjecture?)

(define (conjecture-equal? conj1 conj2)
  (equal? (print conj1)
           (print conj2)))

;;; Whether
(define (satisfies-conjecture conj premise-instances)
  (let ((new-args
         (map
          (lambda (construction-proc)
            (construction-proc premise-instances))
          (conjecture-construction-procedures conj)))
        (rel (conjecture-relationship conj)))
    (or (relationship-holds rel new-args)
        (begin (if *explain*
                   (pprint `(failed-conjecture ,conj)))
               #f))))


(define (conjecture-from-observation obs)
  (make-conjecture
   '()
   (map element-dependencies->list (observation-args obs))
   (map element-source (observation-args obs))
   (observation-relationship obs)))


;;; Removing redundant conjectures

(define (simplify-conjectures conjectures base-conjectures)
  (define memp (member-procedure conjecture-equal?))
  (filter
   (lambda (o) (not (memp o base-conjectures)))
   conjectures))