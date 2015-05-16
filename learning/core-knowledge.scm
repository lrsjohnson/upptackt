;;; core-knowledge.scm -- Core knowledge of a student

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;; Adding to student ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (provide-core-knowledge student)
  (for-each (lambda (def)
              (add-definition! student def))
            primitive-definitions)
  (for-each (lambda (def)
              (add-definition! student def))
            built-in-definitions))

;;;;;;;;;;;;;;;;;;;;;;;;; Primitive definitions ;;;;;;;;;;;;;;;;;;;;;;;;;

(define primitive-definitions
  (list
   (make-primitive-definition 'point point? random-point)
   (make-primitive-definition 'line line? random-line)
   (make-primitive-definition 'segment segment? random-segment)
   (make-primitive-definition 'polygon polygon? random-polygon)
   (make-primitive-definition 'circle circle? random-circle)
   (make-primitive-definition 'angle angle? random-angle)))

;;;;;;;;;;;;;;;;;;;;;;;; Built-in Definitions ;;;;;;;;;;;;;;;;;;;;;;;;

(define built-in-definitions
  (list
   ;; Triangle
   (make-restrictions-definition
    'triangle '(polygon)
    (list (lambda (t) (= (polygon-n-points t) 3)))
    random-triangle)
   ;; Quadrilateral
   (make-restrictions-definition
    'quadrilateral '(polygon)
    (list (lambda (t) (= (polygon-n-points t) 4)))
    random-quadrilateral)

   ;; Isoceles Triangle!
   #|
   (make-restrictions-definition
    'isoceles-triangle 'triangle
    (list (lambda (t)
            (let* ((a (polygon-point-ref t 0))
                   (b (polygon-point-ref t 1))
                   (c (polygon-point-ref t 2)))
              (segment-equal-length? (make-segment a b)
                                     (make-segment a c)))))

   random-isoceles-triangle))
|#
   ))
