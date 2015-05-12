;;; core.scm --- Core definitions used throughout the figure elements

;;; Commentary:

;; Ideas:
;; - Some gemeric handlers used in figure elements

;; Future:
;; - figure-element?, e.g.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;; Element Component ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define element-component
  (make-generic-operation
   2 'element-component
   (lambda (el i)
     (error "No component procedure for element" el))))

(define (component-procedure-from-getters . getters)
  (let ((num-getters (length getters)))
    (lambda (el i)
      (if (not (<= 0 i (- num-getters 1)))
          (error "Index out of range for component procedure: " i))
      ((list-ref getters i)
       el))))

(define (declare-element-component-handler handler type)
  (defhandler element-component handler type number?))

#|
Example Usage:

(declare-element-component-handler
 (component-procedure-from-getters car cdr)
 pair?)

(declare-element-component-handler vector-ref vector?)

(element-component '(3 . 4 ) 1)
;Value: 4

(element-component #(1 2 3) 2)
;Value: 3
|#
