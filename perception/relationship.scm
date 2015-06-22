;;; relationship.scm -- relationships among element-list

;;; Commentary

;; Ideas:
;; - Include with relationship types predicates for how to use them.

;; Future:
;; - Think about procedures / dependencies to obtain arguments

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;; Relationship Structure ;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <relationship>
  (make-relationship name arity predicate equivalence-predicate)
  relationship?
  (name relationship-name)
  (arity relationship-arity)
  (predicate relationship-predicate)
  (equivalence-predicate relationship-equivalence-predicate))

(define (relationship-equivalent? r1 r2)
  (eq? (relationship-name r1)
       (relationship-name r2)))

(define print-relationship relationship-name)

(defhandler print print-relationship relationship?)

;;;;;;;;;;;;;;;;;;;;;;; Checking relationships ;;;;;;;;;;;;;;;;;;;;;;;

(define (relationship-holds r element-list)
  (apply (relationship-predicate r) element-list))

;;;;;;;;;;;;;;;;;;;;;;;;; Basic relationship ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Segments:

(define equal-length-relationship
  (make-relationship 'equal-length 2 segment-equal-length?
                      (set-equivalent-procedure segment-equivalent?)))

;;; Angles:
(define equal-angle-relationship
  (make-relationship 'equal-angle 2 angle-measure-equal?
                      (set-equivalent-procedure angle-equivalent?)))

(define supplementary-angles-relationship
  (make-relationship 'supplementary 2 supplementary-angles?
                      (set-equivalent-procedure angle-equivalent?)))

(define complementary-angles-relationship
  (make-relationship 'complementary 2 complementary-angles?
                      (set-equivalent-procedure angle-equivalent?)))

;;; Linear elements:
(define perpendicular-relationship
  (make-relationship 'perpendicular 2 perpendicular?
                      (set-equivalent-procedure linear-element-equivalent?)))

(define parallel-relationship
  (make-relationship 'parallel 2 parallel?
                      (set-equivalent-procedure linear-element-equivalent?)))

(define concurrent-relationship
  (make-relationship 'concurrent 3 concurrent?
                      (set-equivalent-procedure linear-element-equivalent?)))

;;; Points:
(define concurrent-points-relationship
  (make-relationship 'concurrent 2 point-equal?
                      (set-equivalent-procedure point-equal?)))

(define concentric-relationship
  (make-relationship 'concentric 4 concentric?
                      (set-equivalent-procedure point-equal?)))

(define concentric-with-center-relationship
  (make-relationship 'concentric-with-center
                      4 concentric-with-center?
                      (set-equivalent-procedure point-equal?)))

;;; Polygons:
(define (make-polygon-n-sides-relationship n)
  (make-relationship (symbol 'n-sides- n)
                      1 (ngon-predicate n)
                      eq?))

(define (make-polygon-term-relationship polygon-term)
  (make-relationship polygon-term
                      1
                      (lambda (obj) (is-a? polygon-term obj))
                      eq?))
