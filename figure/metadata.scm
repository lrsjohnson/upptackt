;;; metadata.scm - Element metadata

;;; Commentary:

;; Ideas:
;; - Currently, names
;; - Dependencies grew here, but are now separate

;; Future:
;; - Point/Linear/Circle adjacency - walk like graph

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Names ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-element-name! element name)
  (if (and (named? element)
           (not (eq? (element-name element)
                     name)))
      (error "Reassining element name:"
             (list element (element-name element) name)))
  (eq-put! element 'name name)
  element)

(define (element-name element)
  (or (eq-get element 'name)
      *unnamed*))

(define *unnamed* (list 'unnamed))
(define (is-unnamed? x) (eq? *unnamed* x))

(define generic-element-name
  (make-generic-operation 1 'generic-element-name
                          element-name))

(define (named? element)
  (not (is-unnamed? (element-name element))))
