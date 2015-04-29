;;; constraints.scm --- Constraints for mechanisms

;;; Commentary:

;; Ideas:
;; - Abstraction for specifying constraints
;; - Length, angle equality
;; - Perpendicular / Parellel

;; Future:
;; - Constraints for other linkages?

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;; Constraint Structure ;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <m:constraint>
  (m:make-constraint type args constraint-procedure)
  m:constraint?
  (type m:constraint-type)
  (args m:constraint-args)
  (constraint-procedure m:constraint-procedure))

;;;;;;;;;;;;;;;;;;;;;;;;;; Constraint Types ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:c-length-equal bar-id-1 bar-id-2)
  (m:make-constraint
   'm:c-length-equal
   (list bar-id-1 bar-id-2)
   (lambda (m)
     (let ((bar-1 (m:lookup m bar-id-1))
           (bar-2 (m:lookup m bar-id-2)))
       (c:id
        (m:bar-length bar-1)
        (m:bar-length bar-2))))))

(define (m:c-angle-equal joint-id-1 joint-id-2)
  (m:make-constraint
   'm:c-angle-equal
   (list joint-id-1 joint-id-2)
   (lambda (m)
     (let ((joint-1 (m:lookup m joint-id-1))
           (joint-2 (m:lookup m joint-id-2)))
       (c:id (m:joint-theta joint-1)
             (m:joint-theta joint-2))))))

(define (m:c-right-angle joint-id)
  (m:make-constraint
   'm:right-angle
   (list joint-id)
   (lambda (m)
     (let ((joint (m:lookup m joint-id)))
       (c:id
        (m:joint-theta joint)
        (/ pi 2))))))

;;;;;;;;;;;;; Applying and Marking Constrained Elements ;;;;;;;;;;;;;;

(define (m:constrained? element)
  (not (null? (m:element-constraints element))))

(define (m:element-constraints element)
  (or (eq-get element 'm:constraints)
      '()))

(define (m:set-element-constraints! element constraints)
  (eq-put! element 'm:constraints constraints))

(define (m:mark-constraint element constraint)
  (m:set-element-constraints!
   element
   (cons constraint
         (m:element-constraints element))))

(define (m:apply-constraint m constraint)
  (for-each (lambda (element-id)
              (m:mark-constraint
               (m:lookup m element-id)
               constraint))
            (m:constraint-args constraint))
  ((m:constraint-procedure constraint) m))
