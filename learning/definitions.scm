;;; definitions.scm --- representation and interaction with definitions

;;; Commentary:

;; Ideas:
;; - primitive definitions

;; Future:
;; - relationship-based definitions

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;; Basic Structure ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <definition>
  (%make-definition name classifications conjectures predicate generator)
  definition?
  (name definition-name)
  (classifications definition-classifications %set-definition-classifications!)
  (conjectures definition-conjectures set-definition-conjectures!)
  (predicate definition-predicate set-definition-predicate!)
  (generator definition-generator))

(define (make-primitive-definition name predicate generator)
  (%make-definition name '() '()  predicate generator))

(define (primitive-definition? def)
  (and (definition? def)
       (null? (definition-classifications def))))

;;;;;;;;;;;;;;;;;;;;;;;;; Using Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (definition-holds? def obj)
  (let ((classifications (definition-classifications def))
        (conjectures (definition-conjectures def)))
    (and (every
          (lambda (classification-term)
            (is-a? classification-term obj))
          classifications)
         ((definition-predicate def) obj)
         (every (lambda (conjecture)
                  (satisfies-conjecture? conjecture (list obj)))
                conjectures))))

;;;;;;;;;;;;;;;;;;;;;; Higher-order Definitions ;;;;;;;;;;;;;;;;;;;;;;

(define (make-restrictions-definition
         name classifications conjectures generator)
  (%make-definition name classifications conjectures true-proc generator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Formatting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-definition def)
  (list (definition-name def)
        (definition-classifications def)
        (map print (definition-conjectures def))))

(defhandler print print-definition
  definition?)

(define (print-primitive-definition def)
  'primitive-definition)

(defhandler print print-primitive-definition
  primitive-definition?)
