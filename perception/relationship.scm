;;; relationship.scm -- relationships among element-list

;;; Commentary

;; Ideas:
;; - Include with relationship types predicates for how to use them.

;; Future:
;; - Think about procedures / dependencies to obtain arguments

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;; Relationship Structure ;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <relationship>
  (%make-relationship type arity predicate)
  relationship?
  (type relationship-type)
  (arity relationship-arity)
  (predicate relationship-predicate))

(define print-relationship relationship-type)

(defhandler print print-relationship relationship?)

;;;;;;;;;;;;;;;;;;;;;;; Checking relationships ;;;;;;;;;;;;;;;;;;;;;;;

(define (relationship-holds r element-list)
  (apply (relationship-predicate r) element-list))

;;;;;;;;;;;;;;;;;;;;;;;;; Basic relationship ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Segments:

(define equal-length-relationship
  (%make-relationship 'equal-length 2 segment-equal-length?))

;;; Angles:
(define equal-angle-relationship
  (%make-relationship 'equal-angle 2 angle-measure-equal?))

(define supplementary-angles-relationship
  (%make-relationship 'supplementary 2 supplementary-angles?))

(define complementary-angles-relationship
  (%make-relationship 'complementary 2 complementary-angles?))

;;; Linear elements:
(define perpendicular-relationship
  (%make-relationship 'perpendicular 2 perpendicular?))

(define parallel-relationship
  (%make-relationship 'parallel 2 parallel?))

(define concurrent-relationship
  (%make-relationship 'concurrent 3 concurrent?))

;;; Points:
(define concurrent-points-relationship
  (%make-relationship 'concurrent 2 point-equal?))

(define concentric-relationship
  (%make-relationship 'concentric 4 concentric?))

(define concentric-with-center-relationship
  (%make-relationship 'concentric-with-center
                      4 concentric-with-center?))

;;; Polygons:
(define (make-polygon-n-sides-relationship n)
  (%make-relationship (symbol 'n-sides- n)
                      1 (ngon-predicate n)))
