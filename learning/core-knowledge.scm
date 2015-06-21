;;; core-knowledge.scm -- Core knowledge of a student

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;; Adding to student ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (provide-core-knowledge)
  (for-each add-definition! primitive-definitions))

;;;;;;;;;;;;;;;;;;;;;;;;; Primitive definitions ;;;;;;;;;;;;;;;;;;;;;;;;;

(define triangle? (ngon-predicate 3))
(define quadrilateral? (ngon-predicate 4))

(define primitive-definitions
  (list
   (make-primitive-definition 'object true-proc true-proc)
   (make-primitive-definition 'point point? random-point)
   (make-primitive-definition 'line line? random-line)
   (make-primitive-definition 'ray ray? random-ray)
   (make-primitive-definition 'segment segment? random-segment)
   (make-primitive-definition 'polygon polygon? random-polygon)
   (make-primitive-definition 'circle circle? random-circle)
   (make-primitive-definition 'angle angle? random-angle)
   (make-primitive-definition 'triangle triangle?
                              random-triangle)
   (make-primitive-definition 'quadrilateral quadrilateral?
                              random-quadrilateral)))

(define primitive-terms (map definition-name primitive-definitions))
