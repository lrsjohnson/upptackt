;;; definitions.scm --- representation and interaction with definitions

;;; Commentary:

;; Ideas:
;; - primitive definitions

;; Future:
;; - relationship-based definitions

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;; Basic Structure ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <definition>
  (%make-definition name
                    generator
                    predicate
                    primitive?
                    all-conjectures
                    classifications
                    specific-conjectures)
  definition?
  (name definition-name)
  (generator definition-generator)
  (predicate definition-predicate set-definition-predicate!)
  (primitive? definition-primitive?)
  (all-conjectures definition-conjectures set-definition-conjectures!)
  (classifications definition-classifications
                   set-definition-classifications!)
  (specific-conjectures definition-specific-conjectures
                        set-definition-specific-conjectures!))

(define (make-primitive-definition name predicate generator)
  (%make-definition name generator predicate #t '() '() '()))

(define (primitive-definition? def)
  (and (definition? def)
       (definition-primitive? def)))

;;;;;;;;;;;;;;;;;;;;;;;;; Using Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (definition-holds? def obj)
  (let ((classifications (definition-classifications def))
        (specific-conjectures (definition-specific-conjectures def)))
    (and ((definition-predicate def) obj)
         (every
          (lambda (classification-term)
            (is-a? classification-term obj))
          classifications)
         (every (lambda (conjecture)
                  (satisfies-conjecture? conjecture (list obj)))
                specific-conjectures))))

(define (definition-holds-nonrecursive? def obj)
  (let ((all-conjectures (definition-conjectures def)))
    (and ((definition-predicate def) obj)
         (every (lambda (conjecture)
                  (satisfies-conjecture? conjecture (list obj)))
                all-conjectures))))

;;;;;;;;;;;;;;;;;;;;;; Higher-order Definitions ;;;;;;;;;;;;;;;;;;;;;;

(define (make-restrictions-definition
         name
         generator
         primitive-predicate
         conjectures)
  (%make-definition name
                    generator
                    primitive-predicate
                    #f
                    conjectures
                    '()
                    '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Formatting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-definition def)
  (list (definition-name def)
        (definition-classifications def)
        (map print (definition-specific-conjectures def))))

(defhandler print print-definition
  definition?)

(define (print-primitive-definition def)
  `(primitive-definition ,(definition-term def)))

(defhandler print print-primitive-definition
  primitive-definition?)
