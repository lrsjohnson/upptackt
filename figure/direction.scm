;;; direction.scm --- Low-level direction structure

;;; Commentary:

;; A Direction is equivalent to a unit vector pointing in some direction.

;; Ideas:
;; - Ensures range [0, 2pi]
;; - Structure could allow for better propagation/information in future vs.
;;     constants

;; Future:
;; - Could generalize to dx, dy or theta
;; - Direction ranges

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;; Direction Structure ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <direction>
  (%direction theta)
  direction?
  (theta direction-theta))

(define (make-direction theta)
  (%direction (fix-angle-0-2pi theta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Arithemtic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-to-direction dir radians)
  (make-direction (+ (direction-theta dir)
                     radians)))
;;; D2 - D1
(define (subtract-directions d2 d1)
  (fix-angle-0-2pi (- (direction-theta d2)
                      (direction-theta d1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CCW
(define (rotate-direction-90 dir)
  (add-to-direction dir (/ pi 2)))

(define (reverse-direction dir)
  (add-to-direction dir pi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (direction-equal? d1 d2)
  (close-enuf? (direction-theta d1)
               (direction-theta d2)))

(define (direction-opposite? d1 d2)
  (close-enuf? (direction-theta d1)
               (direction-theta (reverse-direction d2))))

(define (direction-perpendicular? d1 d2)
  (close-enuf?
   (abs (- (direction-theta d1)
           (direction-theta d2)))
   (/ pi 2)))

(define (direction-parallel? d1 d2)
  (or (direction-equal? d1 d2)
      (direction-opposite? d1 d2)))

;;;;;;;;;;;;;;;;;;;;;;;; Direction Intervals ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; "arc" of the circle from start-dir CCW to end-dir
;;; "invalid" allows for "impossible" intervals
(define-record-type <direction-interval>
  (%make-direction-interval start-dir end-dir valid)
  direction-interval?
  (valid direction-interval-valid?)
  (start-dir direction-interval-start)
  (end-dir direction-interval-end))

(define (make-direction-interval start-dir end-dir)
  (%make-direction-interval start-dir end-dir #t))

(define (within-direction-interval? dir dir-interval)
  (let ((dir-start (direction-interval-start dir-interval))
        (dir-end (direction-interval-end dir-interval)))
    (<= (subtract-directions dir dir-start)
        (subtract-directions dir-end dir-start))))

#|
 (define a (make-direction (* 7 (/ pi 4))))
 (define b (make-direction (/ pi 2)))
 (define c (make-direction (/ pi 4)))

 (within-direction-interval? c
  (make-direction-interval a b))
 ;Value: #t

 (within-direction-interval? c
  (make-direction-interval b a))
 ;Value: #f
|#
