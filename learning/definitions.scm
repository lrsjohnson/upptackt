;;; definitions.scm --- representation and interaction with definitions

;;; Commentary:

;; Ideas:
;; - primitive definitions

;; Future:
;; - relationship-based definitions

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;; Basic Structure ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <definition>
  (%make-definition name classification restrictions predicate)
  definition?
  (name definition-name)
  (classification definition-classification)
  (restrictions definition-restrictions)
  (predicate definition-predicate set-definition-predicate!))

(define (make-primitive-definition name predicate)
  (%make-definition name 'primitive '()  predicate))

(define (primitive-definition? def)
  (and (definition? def)
       (eq? 'primitive
            (definition-classification def))))

;;;;;;;;;;;;;;;;;;;;;; Higher-order Definitions ;;;;;;;;;;;;;;;;;;;;;;

(define (make-restrictions-definition name classification restrictions)
  (%make-definition name classification restrictions #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Formatting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-primitive-definition def)
  'primitive-definition)

(defhandler print print-primitive-definition
  primitive-definition?)

(define (print-primitive-definition def)
  'primitive-definition)
