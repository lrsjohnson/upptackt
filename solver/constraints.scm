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
       (c:id (m:bar-length bar-1)
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

;;; p2 between p1 p3 in a line
(define (m:c-line-order p1-id p2-id p3-id)
  (list
   (m:make-named-bar p1-id p2-id)
   (m:make-named-bar p2-id p3-id)
   (m:make-named-joint p1-id p2-id p3-id)
   (m:c-full-angle (m:joint p1-id p2-id p3-id))))

(define (m:c-full-angle joint-id)
  (m:make-constraint
   'm:full-angle
   (list joint-id)
   (lambda (m)
     (let ((joint (m:lookup m joint-id)))
       (c:id
        (m:joint-theta joint)
        pi)))))

(define (m:equal-joints-in-sum equal-joint-ids
                               all-joint-ids
                               total-sum)
  (m:make-constraint
   'm:equal-joints-in-sum
   all-joint-ids
   (lambda (m)
     (let ((all-joints (m:multi-lookup m all-joint-ids))
           (equal-joints (m:multi-lookup m equal-joint-ids)))
       (let ((other-joints
              (set-difference all-joints equal-joints eq?)))
         (c:id (m:joint-theta (car equal-joints))
               (ce:/
                (ce:- total-sum
                      (ce:multi+ (map m:joint-theta other-joints)))
                (length equal-joints))))))))

(define (n-gon-angle-sum n)
  (* n (- pi (/ (* 2 pi) n))))

(define (m:polygon-sum-slice all-joint-ids)
  (m:make-slice
   (m:make-constraint 'm:joint-sum all-joint-ids
    (lambda (m)
      (let ((all-joints (m:multi-lookup m all-joint-ids))
            (total-sum (n-gon-angle-sum (length all-joint-ids))))
        (m:joints-constrained-in-sum all-joints total-sum))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Slices ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Slices are constraints that are processed after the normal
;;; constraints have been aplied.

(define-record-type <m:slice>
  (m:make-slice constraint)
  m:slice?
  (constraint m:slice-constraint))

(define (m:apply-slice m slice)
  (m:apply-constraint m (m:slice-constraint slice)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Propagator Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ce:multi+ cells)
  (cond ((null? cells) 0)
        ((null? (cdr cells)) (car cells))
        (else
         (ce:+ (car cells)
               (ce:multi+ (cdr cells))))))

;;;;;;;;;;;;;;;;;;;;;;;;; Slices (for sums) ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:equal-values-in-sum equal-cells all-cells total-sum)
  (let ((other-values (set-difference all-cells equal-cells eq?)))
    (c:id (car equal-cells)
          (ce:/ (ce:- total-sum (ce:multi+ other-values))
                (length equal-cells)))))

(define (m:sum-slice elements cell-transformer equality-predicate total-sum)
  (let* ((equivalence-classes
          (partition-into-equivalence-classes elements equality-predicate))
         (nonsingular-classes (filter nonsingular? equivalence-classes))
         (all-cells (map cell-transformer elements)))
    (cons (c:id total-sum (ce:multi+ all-cells))
          (map (lambda (equiv-class)
                 (m:equal-values-in-sum
                  (map cell-transformer equiv-class) all-cells total-sum))
               equivalence-classes))))

(define (nonsingular? equivalence-class)
  (> (length equivalence-class) 1))

(define (angle-equal-constraint? c)
  (eq? (m:constraint-type c) 'm:c-angle-equal))

(define (m:joints-constrained-equal-to-one-another? joint-1 joint-2)
  (let ((joint-1-constraints
         (filter angle-equal-constraint?
                 (m:element-constraints joint-1)))
        (joint-2-constraints
         (filter angle-equal-constraint?
                 (m:element-constraints joint-2))))
    (not (null? (set-intersection joint-1-constraints
                                  joint-2-constraints
                                  (member-procedure eq?))))))

(define (m:joints-constrained-in-sum all-joints total-sum)
  (m:sum-slice
   all-joints
   m:joint-theta
   m:joints-constrained-equal-to-one-another?
   total-sum))
