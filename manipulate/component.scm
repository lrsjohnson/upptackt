;;; To begin, we will only support edges of polygons
;;; Need to think more about this...

;;; A Component is the abstraction of a group of geometry elements that can be moved together

;;; A rigid component has all

(define (point-connection))

(define (variable-length-segment))

(define (constrain-segment-equal))

(define (constrain-angle-equal))

(define (variable-angle))

;;; Merging Components:

(define (merge-component))
;;; (1) Find two point-connections in separate components with the same name
;;; (2) Translate to align these point connections
;;; (3) Mark the second component as extending from the first at that merged point-connection via some arbitrary angle
;;; (4) For each new pair of points, check if their distance has been established in any other diagram, add "constraints"
;;;       or try to rotate the new angle to satisfy such

;;; Rotate + extend one length? [extend length for desired distance, then rotate to meet desired point]

;;; (4) Satisfy other point connections in the diagram:
;;;     - Check distances between point connections in the components


;;; Multiple angles with equality???
