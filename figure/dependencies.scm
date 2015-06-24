;;; dependencies.scm --- Dependencies of figure elements

;;; Commentary:

;; Ideas:
;; - Use eq-properties to set dependencies of elements
;; - Some random elements are gien external/random dependencies
;; - For some figures, override dependencies of intermediate elements

;; Future:
;; - Expand to full dependencies
;; - Start "learning" and generalizing

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Sources ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-source! element source)
  (eq-put! element 'source source))

(define (with-source source element)
  (set-source! element (memoize-function source))
  element)

(define (element-source element)
  (or (eq-get element 'source)
      (lambda (p) element)))

(define (from-new-premise new-premise element)
  ((element-source element)
   new-premise))

;;;;;;;;;;;;;;;;;;;;;;;; Setitng Dependencies ;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-dependency! element dependency)
  (if (not (number? element))
      (eq-put! element 'dependency dependency)))

(define (set-dependency-if-unknown! element dependency)
  (if (dependency-unknown? element)
      (set-dependency! element dependency)))

(define (with-dependency dependency element)
  (set-dependency! element dependency)
  element)


(define (with-dependency-if-unknown dependency element)
  (if (dependency-unknown? element)
      (with-dependency dependency element)
      element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Premises ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-as-premise! element i)
  (set-dependency! element (symbol '<premise- i '>))
  (set-source! element (lambda (p) (list-ref p i))))

(define (as-premise element i)
  (set-as-premise! element i)
  element)

;;;;;;;;;;;;;;;;;;;;;;;; Unknown Dependencies ;;;;;;;;;;;;;;;;;;;;;;;;

(define (dependency-known? element)
  (eq-get element 'dependency))

(define dependency-unknown? (notp dependency-known?))

(define (clear-dependency! element)
  (set-dependency! element #f))

;;;;;;;;;;;;;;;;;;;;;;; Accessing Dependencies ;;;;;;;;;;;;;;;;;;;;;;;

(define (element-dependency element)
  (or (eq-get element 'dependency)
      element))

(define element-dependencies->list
  (make-generic-operation
   1 'element-dependencies->list
   (lambda (x) x)))

(define (element-dependency->list el)
  (element-dependencies->list
   (element-dependency el)))

(defhandler element-dependencies->list
  element-dependency->list
  dependency-known?)

(defhandler element-dependencies->list
  (lambda (l)
    (map element-dependencies->list l))
  list?)

;;;;;;;;;;;;;;;;;;;;;; Formatting Dependencies ;;;;;;;;;;;;;;;;;;;;;;;

(define (print-dependencies object)
  (pprint (element-dependencies->list object)))
