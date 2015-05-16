;;; relationship.scm -- relationships among elements

;;; Commentary

;; Ideas:
;; - Include with relationship types predicates for how to use them.

;; Future:
;; - Think about procedures / dependencies to obtain arguments

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;; Relationship Structure ;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <relationship>
  (%make-relationship type predicate)
  relationship?
  (type relationship-type)
  (predicate relationship-predicate))

(define print-relationship relationship-type)

(defhandler print print-relationship relationship?)

;;;;;;;;;;;;;;;;;;;;;;; Checking relationships ;;;;;;;;;;;;;;;;;;;;;;;

(define (relationship-holds r elements)
  (apply (relationship-predicate r) elements))

;;;;;;;;;;;;;;;;;;;;;;;;; Basic relationship ;;;;;;;;;;;;;;;;;;;;;;;;;

(define equal-length-relationship
  (%make-relationship 'equal-length segment-equal-length?))

(define equal-angle-relationship
  (%make-relationship 'equal-angle angle-measure-equal?))

(define perpendicular-relationship
  (%make-relationship 'perpendicular perpendicular?))

(define parallel-relationship
  (%make-relationship 'parallel parallel?))

(define concurrent-points-relationship
  (%make-relationship 'concurrent point-equal?))

(define supplementary-angles-relationship
  (%make-relationship 'supplementary supplementary-angles?))

(define complementary-angles-relationship
  (%make-relationship 'complementary complementary-angles?))
