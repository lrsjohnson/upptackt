;;; direction.scm --- Low-level direction structure

;;; Commentary:

;; A Direction is equivalent to a unit vector pointing in some direction.

;; Ideas:
;; - Ensures range [0, 2pi]

;; Future:
;; - Could generalize to dx, dy or theta

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;; Direction Structure ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <direction>
  (%direction theta)
  direction?
  (theta direction-theta))

(define (make-direction theta)
  (%direction (fix-angle-0-2pi theta)))

(define (print-direction dir)
  `(direction ,(direction-theta dir)))
(defhandler print print-direction direction?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Arithemtic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-to-direction dir radians)
  (make-direction (+ (direction-theta dir)
                     radians)))
;;; D2 - D1
(define (subtract-directions d2 d1)
  (if (direction-equal? d1 d2)
      0
      (fix-angle-0-2pi (- (direction-theta d2)
                          (direction-theta d1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CCW
(define (rotate-direction-90 dir)
  (add-to-direction dir (/ pi 2)))

(define (reverse-direction dir)
  (add-to-direction dir pi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (direction-equal? d1 d2)
  (or (close-enuf? (direction-theta d1)
                   (direction-theta d2))
      (close-enuf? (direction-theta (reverse-direction d1))
                   (direction-theta (reverse-direction d2)))))

(define (direction-opposite? d1 d2)
  (close-enuf? (direction-theta d1)
               (direction-theta (reverse-direction d2))))

(define (direction-perpendicular? d1 d2)
  (let ((difference (subtract-directions d1 d2)))
    (or (close-enuf? difference (/ pi 2))
        (close-enuf? difference (* 3 (/ pi 2))))))

(define (direction-parallel? d1 d2)
  (or (direction-equal? d1 d2)
      (direction-opposite? d1 d2)))
