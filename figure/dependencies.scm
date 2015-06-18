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
  (eq-put! element 'dependency dependency))

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
;;;;;;;;;;;;;;;;;;;;;;;; Unknown Dependencies ;;;;;;;;;;;;;;;;;;;;;;;;

(define (dependency-known? element)
  (eq-get element 'dependency))

(define dependency-unknown? (notp dependency-known?))

;;;;;;;;;;;;;;;;;;;;;;; Accessing Dependencies ;;;;;;;;;;;;;;;;;;;;;;;

(define (element-dependency element)
  (or (eq-get element 'dependency)
      element))

;;;;;;;;;;;;;;;;;;;;;;;; Random Dependencies ;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-random-dependency tag)
  (%make-random-dependency tag 0))

(define-record-type <random-dependency>
  (%make-random-dependency tag num)
  random-dependency?
  (tag random-dependency-tag)
  (num %random-dependency-num set-random-dependency-num!))

(define (random-dependency-num rd)
  (let ((v (%random-dependency-num rd)))
    (if (= v 0)
        0
        v)))

(define (print-random-dependency rd)
  (list (random-dependency-tag rd)
        (random-dependency-num rd)))
(defhandler print print-random-dependency random-dependency?)

(define (number-figure-random-dependencies! figure)
  (define *random-dependency-num* 1)
  (map (lambda (el)
         (let ((dep (element-dependency el)))
           (cond ((random-dependency? dep)
                  (set-random-dependency-num!
                   dep
                   *random-dependency-num*)
                  (set! *random-dependency-num*
                        (+ *random-dependency-num* 1))))))
       (figure-elements figure))
  'done)

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
  print-random-dependency
  random-dependency?)

(defhandler element-dependencies->list
  (lambda (l)
    (map element-dependencies->list l))
  list?)




;;;;;;;;;;;;;;;;;;;;;; Formatting Dependencies ;;;;;;;;;;;;;;;;;;;;;;;

(define (format-dependencies object)
  (element-dependencies->list object))
