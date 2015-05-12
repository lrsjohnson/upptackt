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

;;;;;;;;;;;;;;;;;;;;;;;; Setitng Dependencies ;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-dependency! element dependency)
  (eq-put! element 'dependency dependency))

(define (with-dependency dependency element)
  (set-dependency! element dependency)
  element)

(define (with-dependency-if-unknown dependency element)
  (if (dependency-unknown? element)
      (with-dependency dependency element)
      element))

;;;;;;;;;;;;;;;;;;;;;;;; Unknown Dependencies ;;;;;;;;;;;;;;;;;;;;;;;;

(define *unknown-dependency* (list '*unknown-dependency*))
(define (unknown-dependency? x)
  (eq? x *unknown-dependency*))

(define (dependency-unknown? element)
  (unknown-dependency? (element-dependency element)))

(define dependency-known? (notp dependency-unknown?))
;;;;;;;;;;;;;;;;;;;;;;; Accessing Dependencies ;;;;;;;;;;;;;;;;;;;;;;;

(define (element-dependency element)
  (or (eq-get element 'dependency)
      *unknown-dependency*))

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
