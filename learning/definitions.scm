;;; definitions.scm --- representation and interaction with definitions

;;; Commentary:

;; Ideas:
;; - primitive definitions

;; Future:
;; - relationship-based definitions

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;; Basic Structure ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <definition>
  (%make-definition name classifications observations predicate generator)
  definition?
  (name definition-name)
  (classifications definition-classifications)
  (observations definition-observations)
  (predicate definition-predicate set-definition-predicate!)
  (generator definition-generator))

(define (make-primitive-definition name predicate generator)
  (%make-definition name '() '()  predicate generator))

(define (primitive-definition? def)
  (and (definition? def)
       (null? (definition-classifications def))))

;;;;;;;;;;;;;;;;;;;;;; Higher-order Definitions ;;;;;;;;;;;;;;;;;;;;;;

(define (make-restrictions-definition
         name classifications observations generator)
  (%make-definition name classifications observations #f generator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Formatting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-definition def)
  (list (definition-name def)
        (definition-classifications def)
        (map print (definition-observations def))))

(defhandler print print-definition
  definition?)

(define (print-primitive-definition def)
  'primitive-definition)

(defhandler print print-primitive-definition
  primitive-definition?)
